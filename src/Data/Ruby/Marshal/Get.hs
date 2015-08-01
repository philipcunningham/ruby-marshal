{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.Ruby.Marshal.Get
-- Copyright : (c) Philip Cunningham, 2015
-- License   : MIT
--
-- Maintainer:  802389+philipcunningham@users.noreply.github.com
-- Stability :  experimental
-- Portability: portable
--
-- Ruby Marshal deserialiser using @Data.Serialize@.
--
--------------------------------------------------------------------

module Data.Ruby.Marshal.Get (
    getMarshalVersion
  , getRubyObject
) where

import Control.Applicative
import Data.Ruby.Marshal.Internal.Int
import Data.Ruby.Marshal.Types

import Control.Monad              (liftM2)
import Control.Monad.State        (get, gets, put)
import Data.Ruby.Marshal.Encoding (toEnc)
import Data.Ruby.Marshal.Monad    (_objects, _symbols, liftMarshal)
import Data.Serialize.Get         (Get, getBytes, getTwoOf, label)
import Data.String.Conv           (toS)
import Text.Read                  (readMaybe)

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

import Prelude hiding (length)

--------------------------------------------------------------------
-- Top-level functions.

-- | Deserialises Marshal version.
getMarshalVersion :: Marshal (Word8, Word8)
getMarshalVersion = marshalLabel "Marshal Version" $
  getTwoOf getWord8 getWord8

-- | Deserialises a subset of Ruby objects.
getRubyObject :: Marshal RubyObject
getRubyObject = getMarshalVersion >> go
  where
    go :: Marshal RubyObject
    go = liftMarshal getWord8 >>= \case
      NilChar        -> return RNil
      TrueChar       -> return $ RBool True
      FalseChar      -> return $ RBool False
      ArrayChar      -> RArray  <$> getArray go
      FixnumChar     -> RFixnum <$> getFixnum
      FloatChar      -> RFloat  <$> getFloat
      HashChar       -> RHash   <$> getHash go go
      IVarChar       -> RIVar   <$> getIVar go
      ObjectLinkChar -> RIVar   <$> getObjectLink
      StringChar     -> RString <$> getString
      SymbolChar     -> RSymbol <$> getSymbol
      SymlinkChar    -> RSymbol <$> getSymlink
      _              -> return $ Unsupported

--------------------------------------------------------------------
-- Ancillary functions.

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Array.html Array>.
getArray :: Marshal a -> Marshal (V.Vector a)
getArray g = do
  n <- getFixnum
  x <- V.replicateM n g
  marshalLabel "Array" $ return x

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Fixnum.html Fixnum>.
getFixnum :: Marshal Int
getFixnum = marshalLabel "Fixnum" $ do
  x <- getInt8
  if | x ==  0   -> fromIntegral <$> return x
     | x ==  1   -> fromIntegral <$> getWord8
     | x == -1   -> fromIntegral <$> getNegInt16
     | x ==  2   -> fromIntegral <$> getWord16le
     | x == -2   -> fromIntegral <$> getInt16le
     | x ==  3   -> fromIntegral <$> getWord24le
     | x == -3   -> fromIntegral <$> getInt24le
     | x ==  4   -> fromIntegral <$> getWord32le
     | x == -4   -> fromIntegral <$> getInt32le
     | x >=  6   -> fromIntegral <$> return (x - 5)
     | x <= -6   -> fromIntegral <$> return (x + 5)
     | otherwise -> empty
  where
    getNegInt16 :: Get Int16
    getNegInt16 =  do
      x <- fromIntegral <$> getInt8
      if x >= 0 && x <= 127 then return (x - 256) else return x

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Float.html Float>.
getFloat :: Marshal Float
getFloat = do
  s <- getString
  x <- case readMaybe . toS $ s of
    Just float -> return float
    Nothing    -> fail "getFloat: expected float"
  marshalLabel "Float" $ return x

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Hash.html Hash>.
getHash :: Marshal a -> Marshal b -> Marshal (V.Vector (a, b))
getHash k v = do
  n <- getFixnum
  x <- V.replicateM n (liftM2 (,) k v)
  marshalLabel "Hash" $ return x

-- | Deserialises <http://docs.ruby-lang.org/en/2.1.0/marshal_rdoc.html#label-Instance+Variables Instance Variables>.
getIVar :: Marshal RubyObject -> Marshal (RubyObject, REncoding)
getIVar g = do
  string <- g
  length <- getFixnum
  if | length /= 1 -> fail "getIvar: expected single character"
     | otherwise   -> do
       symbol <- g
       denote <- g
       case symbol of
         RSymbol "E" -> case denote of
           RBool True  -> cacheAndReturn string UTF_8
           RBool False -> cacheAndReturn string US_ASCII
           _           -> fail "getIVar: expected bool"
         RSymbol "encoding" -> case denote of
           RString enc -> cacheAndReturn string (toEnc enc)
           _           -> fail "getIVar: expected string"
         _          -> fail "getIVar: invalid ivar"
  where
    cacheAndReturn string enc = do
      let result = (string, enc)
      writeCache $ RIVar result
      marshalLabel "IVar" $ return result

-- | Pulls an Instance Variable out of the object cache.
getObjectLink :: Marshal (RubyObject, REncoding)
getObjectLink = do
  index <- getFixnum
  maybeObject <- readObject index
  case maybeObject of
    Just (RIVar x) -> return x
    _              -> fail "getObjectLink"

-- | Deserialises <http://ruby-doc.org/core-2.2.0/String.html String>.
getString :: Marshal BS.ByteString
getString = do
  n <- getFixnum
  x <- liftMarshal $ getBytes n
  marshalLabel "RawString" $ return x

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Symbol.html Symbol>.
getSymbol :: Marshal BS.ByteString
getSymbol = do
  x <- getString
  writeCache $ RSymbol x
  marshalLabel "Symbol" $ return x

-- | Pulls a Symbol out of the symbol cache.
getSymlink :: Marshal BS.ByteString
getSymlink = do
  index <- getFixnum
  maybeObject <- readSymbol index
  case maybeObject of
    Just (RSymbol bs) -> return bs
    _                 -> fail "getSymlink"

--------------------------------------------------------------------
-- Utility functions.

-- | Lift label into Marshal monad.
marshalLabel :: String -> Get a -> Marshal a
marshalLabel x y = liftMarshal $ label x y

-- | Look up object in our object cache.
readObject :: Int -> Marshal (Maybe RubyObject)
readObject index = gets _objects >>= \objectCache ->
  return $ objectCache V.!? index

-- | Look up a symbol in our symbol cache.
readSymbol :: Int -> Marshal (Maybe RubyObject)
readSymbol index = gets _symbols >>= \symbolCache ->
  return $ symbolCache V.!? index

-- | Write an object to the appropriate cache.
writeCache :: RubyObject -> Marshal ()
writeCache object = do
  cache <- get
  case object of
    RIVar   _ -> put $ cache { _objects = V.snoc (_objects cache) object }
    RSymbol _ -> put $ cache { _symbols = V.snoc (_symbols cache) object }
    _         -> return ()

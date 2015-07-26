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
  , getNil
  , getBool
  , getArray
  , getFixnum
  , getFloat
  , getHash
  , getIvar
  , getObjectLink
  , getString
  , getSymbol
  , getSymlink
) where

import Control.Applicative
import Data.Ruby.Marshal.Internal.Int
import Data.Ruby.Marshal.Types

import Control.Monad       (guard, liftM2)
import Control.Monad.State (get, gets, put)
import Data.Serialize.Get  (Get, getBytes, getTwoOf, label)
import Data.String.Conv    (toS)
import Text.Read           (readMaybe)

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
      NilC        -> return RNil
      TrueC       -> return $ RBool True
      FalseC      -> return $ RBool False
      ArrayC      -> RArray  <$> getArray go
      FixnumC     -> RFixnum <$> getFixnum
      FloatC      -> RFloat  <$> getFloat
      HashC       -> RHash   <$> getHash go go
      IvarC       -> RIvar   <$> getIvar go
      ObjectLinkC -> RIvar   <$> getObjectLink
      StringC     -> RString <$> getString
      SymbolC     -> RSymbol <$> getSymbol
      SymlinkC    -> RSymbol <$> getSymlink
      _           -> return $ RError Unsupported

--------------------------------------------------------------------
-- Ancillary functions.

-- | Deserialises <http://ruby-doc.org/core-2.2.0/NilClass.html nil>.
getNil :: Marshal ()
getNil = marshalLabel "Nil" $ tag 48

-- | Deserialises <http://ruby-doc.org/core-2.2.0/TrueClass.html true> and
-- <http://ruby-doc.org/core-2.2.0/FalseClass.html false>.
getBool :: Marshal Bool
getBool = marshalLabel "Bool" $
  True <$ tag 84 <|> False <$ tag 70

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
getFloat :: Marshal Double
getFloat = do
  s <- getString
  x <- case readMaybe . toS $ s of
    Just float -> return float
    Nothing    -> fail "getFloat"
  marshalLabel "Float" $ return x

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Hash.html Hash>.
getHash :: Marshal a -> Marshal b -> Marshal (V.Vector (a, b))
getHash k v = do
  n <- getFixnum
  x <- V.replicateM n (liftM2 (,) k v)
  marshalLabel "Hash" $ return x

-- | Deserialises <http://docs.ruby-lang.org/en/2.1.0/marshal_rdoc.html#label-Instance+Variables Instance Variables>.
getIvar :: Marshal RubyObject -> Marshal (RubyObject, BS.ByteString)
getIvar g = do
  string <- g
  _      <- getFixnum
  symbol <- g
  denote <- g
  case symbol of
    RSymbol "E" -> case denote of
      RBool True  -> cacheAndReturn string "UTF-8"
      RBool False -> cacheAndReturn string "US-ASCII"
      _           -> fail "getIvar: should be followed by bool"
    RSymbol "encoding" -> case denote of
      RString enc -> cacheAndReturn string enc
      _           -> fail "getIvar: should be followed by string"
    _          -> fail "getIvar: invalid ivar"
  where
    cacheAndReturn string enc = do
      let result = (string, enc)
      writeObject $ RIvar result
      marshalLabel "IVar" $ return result

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Symbol.html Symbol>.
getObjectLink :: Marshal (RubyObject, BS.ByteString)
getObjectLink = do
  index <- getFixnum
  maybeObject <- readObject index
  case maybeObject of
    Just (RIvar x) -> return x
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
  writeObject $ RSymbol x
  marshalLabel "Symbol" $ return x

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Symbol.html Symbol>.
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

-- | Guard against invalid input.
tag :: Word8 -> Get ()
tag t = label "Tag" $
  getWord8 >>= \b -> guard $ t == b

-- | Look up object in our object cache.
readObject :: Int -> Marshal (Maybe RubyObject)
readObject index = gets _objects >>= \objectCache ->
  return $ objectCache V.!? index

-- | Look up a symbol in our symbol cache.
readSymbol :: Int -> Marshal (Maybe RubyObject)
readSymbol index = gets _symbols >>= \symbolCache ->
  return $ symbolCache V.!? index

-- | Write an object to the appropriate cache.
writeObject :: RubyObject -> Marshal ()
writeObject object = do
  cache <- get
  case object of
    RIvar   _ -> put $ cache { _objects = V.snoc (_objects cache) object }
    RSymbol _ -> put $ cache { _symbols = V.snoc (_symbols cache) object }
    _         -> return ()

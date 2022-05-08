{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

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
-- Parsers for Ruby Marshal format.
--
--------------------------------------------------------------------

module Data.Ruby.Marshal.Get (
    -- * Ruby Marshal parsers
    getMarshalVersion
  , getRubyObject
) where

import           Control.Applicative
import           Control.Monad              (liftM2, when)
import           Data.Monoid                ((<>))
import qualified Data.ByteString            as BS
import           Data.Ruby.Marshal.Encoding (toEnc)
import           Data.Ruby.Marshal.Int
import           Data.Ruby.Marshal.Monad    (liftMarshal, readObject,
                                             readSymbol, writeCache)
import           Data.Ruby.Marshal.Types
import           Data.Serialize.Get         (Get, getBytes, getTwoOf, label)
import           Data.String.Conv           (toS)
import qualified Data.Vector                as V
import           Prelude
import           Text.Read                  (readMaybe)

--------------------------------------------------------------------
-- Top-level functions.

-- | Parses Marshal version.
getMarshalVersion :: Marshal (Word8, Word8)
getMarshalVersion = liftAndLabel "Marshal Version" $
  getTwoOf getWord8 getWord8 >>= \version -> case version of
    (4, 8) -> return version
    _      -> fail "marshal version unsupported"

-- | Parses a subset of Ruby objects.
getRubyObject :: Marshal RubyObject
getRubyObject = getMarshalVersion >> go
  where
    go :: Marshal RubyObject
    go = liftMarshal getWord8 >>= \case
           NilChar        -> return RNil
           TrueChar       -> return $ RBool True
           FalseChar      -> return $ RBool False
           FixnumChar     -> RFixnum <$> getFixnum
           FloatChar      -> RFloat <$> getFloat
           StringChar     -> RString <$> getString
           SymbolChar     -> RSymbol <$> getSymbol
           ObjectLinkChar -> getObjectLink
           SymlinkChar    -> RSymbol <$> getSymlink
           ArrayChar      -> do
             result <- RArray <$> getArray go
             writeCache result
             pure result
           HashChar       -> do
             result <- RHash <$> getHash go go
             writeCache result
             pure result
           IVarChar       -> RIVar <$> getIVar go
           _              -> return Unsupported

--------------------------------------------------------------------
-- Ancillary functions.

-- | Parses <http://ruby-doc.org/core-2.2.0/Array.html Array>.
getArray :: Marshal a -> Marshal (V.Vector a)
getArray g = marshalLabel "Fixnum" $ do
  n <- getFixnum
  V.replicateM n g

-- | Parses <http://ruby-doc.org/core-2.2.0/Fixnum.html Fixnum>.
getFixnum :: Marshal Int
getFixnum = liftAndLabel "Fixnum" $ do
  x <- getInt8
  if | x == 0 -> fromIntegral <$> return x
     | x == 1 -> fromIntegral <$> getWord8
     | x == -1 -> fromIntegral <$> getNegInt16
     | x == 2 -> fromIntegral <$> getWord16le
     | x == -2 -> fromIntegral <$> getInt16le
     | x == 3 -> fromIntegral <$> getWord24le
     | x == -3 -> fromIntegral <$> getInt24le
     | x == 4 -> fromIntegral <$> getWord32le
     | x == -4 -> fromIntegral <$> getInt32le
     | x >= 6 -> fromIntegral <$> return (x - 5)
     | x <= -6 -> fromIntegral <$> return (x + 5)
     | otherwise -> empty
  where
    getNegInt16 :: Get Int16
    getNegInt16 = do
      x <- fromIntegral <$> getInt8
      if x >= 0 && x <= 127
        then return (x - 256)
        else return x

-- | Parses <http://ruby-doc.org/core-2.2.0/Float.html Float>.
getFloat :: Marshal Float
getFloat = marshalLabel "Float" $ do
  s <- getString
  case readMaybe . toS $ s of
    Just float -> return float
    Nothing    -> fail "expected float"

-- | Parses <http://ruby-doc.org/core-2.2.0/Hash.html Hash>.
getHash :: Marshal a -> Marshal b -> Marshal (V.Vector (a, b))
getHash k v = marshalLabel "Hash" $ do
  n <- getFixnum
  V.replicateM n (liftM2 (,) k v)

-- | Parses <http://docs.ruby-lang.org/en/2.1.0/marshal_rdoc.html#label-Instance+Variables Instance Variables>.
getIVar :: Marshal RubyObject -> Marshal (RubyObject, RubyStringEncoding)
getIVar g = marshalLabel "IVar" $ do
  str <- g
  len <- getFixnum
  if | len /= 1 -> fail "expected single character"
     | otherwise -> do
        symbol <- g
        denote <- g
        case symbol of
          RSymbol "E" ->
            case denote of
              RBool True  -> return' (str, UTF_8)
              RBool False -> return' (str, US_ASCII)
              _           -> fail "expected bool"
          RSymbol "encoding" ->
            case denote of
              RString enc -> return' (str, toEnc enc)
              _           -> fail "expected string"
          _ -> fail "invalid ivar"
  where
    return' result = do
      writeCache $ RIVar result
      return result

-- | Pulls an Instance Variable out of the object cache.
getObjectLink :: Marshal RubyObject
getObjectLink = marshalLabel "ObjectLink" $ do
  index <- getFixnum
  when (index == 0) $ fail $ "invalid object link (index=0)"
  maybeObject <- readObject (index - 1)
  case maybeObject of
    Just x -> return x
    x      -> fail $ "invalid object link (index=" <> show index <> ", target=" <> show x <> ")"

-- | Parses <http://ruby-doc.org/core-2.2.0/String.html String>.
getString :: Marshal BS.ByteString
getString = marshalLabel "RawString" $ do
  n <- getFixnum
  liftMarshal $ getBytes n

-- | Parses <http://ruby-doc.org/core-2.2.0/Symbol.html Symbol>.
getSymbol :: Marshal BS.ByteString
getSymbol = marshalLabel "Symbol" $ do
  x <- getString
  writeCache $ RSymbol x
  return x

-- | Pulls a Symbol out of the symbol cache.
getSymlink :: Marshal BS.ByteString
getSymlink = marshalLabel "Symlink" $ do
  index <- getFixnum
  maybeObject <- readSymbol index
  case maybeObject of
    Just (RSymbol bs) -> return bs
    _                 -> fail "invalid symlink"

--------------------------------------------------------------------
-- Utility functions.

-- | Lift Get into Marshal monad and then label.
liftAndLabel :: String -> Get a -> Marshal a
liftAndLabel x y = liftMarshal $! label x y

-- | Label underlying Get in Marshal monad.
marshalLabel :: String -> Marshal a -> Marshal a
marshalLabel x y = y >>= \y' -> liftMarshal $! label x (return y')

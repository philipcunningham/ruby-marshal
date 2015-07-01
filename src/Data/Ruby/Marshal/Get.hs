{-# LANGUAGE MultiWayIf #-}

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
  getMarshalVersion,
  getNil,
  getBool,
  getFixnum,
  getArray,
  getHash,
  getString,
  getFloat,
  getSymbol
) where

import Control.Applicative
import Data.Ruby.Marshal.Internal.Int

import Control.Monad      (guard)
import Data.Serialize.Get (Get, getBytes, getTwoOf, label)
import Data.String.Conv   (toS)
import Text.Read          (readMaybe)
import Prelude

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

-- | Deserialises Marshal version.
getMarshalVersion :: Get (Word8, Word8)
getMarshalVersion = label "Marshal Version" $
  getTwoOf getWord8 getWord8

-- | Deserialises <http://ruby-doc.org/core-2.2.0/NilClass.html nil>.
getNil :: Get ()
getNil = label "Nil" $ tag 48

-- | Deserialises <http://ruby-doc.org/core-2.2.0/TrueClass.html true> and
-- <http://ruby-doc.org/core-2.2.0/FalseClass.html false>.
getBool :: Get Bool
getBool = label "Bool" $
  True <$ tag 84 <|> False <$ tag 70

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Fixnum.html Fixnum>.
getFixnum :: Get Int
getFixnum = label "Fixnum" $ do
  x <- getInt8
  if | x ==  0   -> fromIntegral <$> return x
     | x >=  6   -> fromIntegral <$> return (x - 5)
     | x <= -6   -> fromIntegral <$> return (x + 5)
     | x ==  1   -> fromIntegral <$> getWord8
     | x ==  2   -> fromIntegral <$> getWord16le
     | x ==  3   -> fromIntegral <$> getWord24le
     | x ==  4   -> fromIntegral <$> getWord32le
     | x == -1   -> fromIntegral <$> getNegInt16
     | x == -2   -> fromIntegral <$> getInt16le
     | x == -3   -> fromIntegral <$> getInt24le
     | x == -4   -> fromIntegral <$> getInt32le
     | otherwise -> empty
  where
    getNegInt16 :: Get Int16
    getNegInt16 = do
      x <- fromIntegral <$> getInt8
      if x >= 0 && x <= 127 then return (x - 256) else return x

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Array.html Array>.
getArray :: Get a -> Get (V.Vector a)
getArray g = label "Array" $
  getFixnum >>= \len -> V.replicateM len g

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Hash.html Hash>.
getHash :: Get a -> Get b -> Get (V.Vector (a, b))
getHash k v = label "Hash" $
  getFixnum >>= \len -> V.replicateM len $ getTwoOf k v

-- | Deserialises <http://ruby-doc.org/core-2.2.0/String.html String>.
getString :: Get a -> Get BS.ByteString
getString g = label "String" $
  getRawString <* getEncoding -- For now we just throw away the encoding info.
  where getEncoding = getWord8 >> getWord8 >> getRawString >> g

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Float.html Float>.
getFloat :: Get Double
getFloat = label "Float" $ getRawString >>= \x ->
  case readMaybe . toS $ x of
    Just y  -> return y
    Nothing -> empty

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Symbol.html Symbol>.
getSymbol :: Get BS.ByteString
getSymbol = label "Symbol" getRawString

-- | Gets a raw string.
getRawString :: Get BS.ByteString
getRawString = label "RawString" $
  getFixnum >>= getBytes

-- | Guard against invalid input.
tag :: Word8 -> Get ()
tag t = label "Tag" $
  getWord8 >>= \b -> guard $ t == b

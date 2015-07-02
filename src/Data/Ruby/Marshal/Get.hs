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
import Prelude

import Control.Monad      (guard)
import Data.Serialize.Get (Get, getBytes, getTwoOf, label)
import Data.String.Conv   (toS)
import Text.Read          (readMaybe)

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

-- === Fixnum and long

-- "i" represents a signed 32 bit value using a packed format.  One through five
-- bytes follows the type.  The value loaded will always be a Fixnum.  On
-- 32 bit platforms (where the precision of a Fixnum is less than 32 bits)
-- loading large values will cause overflow on CRuby.

-- The fixnum type is used to represent both ruby Fixnum objects and the sizes of
-- marshaled arrays, hashes, instance variables and other types.  In the
-- following sections "long" will mean the format described below, which supports
-- full 32 bit precision.

-- The first byte has the following special values:

-- "\x00"::
--   The value of the integer is 0.  No bytes follow.

-- "\x01"::
--   The total size of the integer is two bytes.  The following byte is a
--   positive integer in the range of 0 through 255.  Only values between 123
--   and 255 should be represented this way to save bytes.

-- "\xff"::
--   The total size of the integer is two bytes.  The following byte is a
--   negative integer in the range of -1 through -256.

-- "\x02"::
--   The total size of the integer is three bytes.  The following two bytes are a
--   positive little-endian integer.

-- "\xfe"::
--   The total size of the integer is three bytes.  The following two bytes are a
--   negative little-endian integer.

-- "\x03"::
--   The total size of the integer is four bytes.  The following three bytes are
--   a positive little-endian integer.

-- "\xfd"::
--   The total size of the integer is two bytes.  The following three bytes are a
--   negative little-endian integer.

-- "\x04"::
--   The total size of the integer is five bytes.  The following four bytes are a
--   positive little-endian integer.  For compatibility with 32 bit ruby,
--   only Fixnums less than 1073741824 should be represented this way.  For sizes
--   of stream objects full precision may be used.

-- "\xfc"::
--   The total size of the integer is two bytes.  The following four bytes are a
--   negative little-endian integer.  For compatibility with 32 bit ruby,
--   only Fixnums greater than -10737341824 should be represented this way.  For
--   sizes of stream objects full precision may be used.

-- Otherwise the first byte is a sign-extended eight-bit value with an offset.
-- If the value is positive the value is determined by subtracting 5 from the
-- value.  If the value is negative the value is determined by adding 5 to the
-- value.

-- There are multiple representations for many values.  CRuby always outputs the
-- shortest representation possible.

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Fixnum.html Fixnum>.
getFixnum :: Get Int
getFixnum = label "Fixnum" $ do
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

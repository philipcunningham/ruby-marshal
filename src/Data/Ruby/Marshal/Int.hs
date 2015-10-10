--------------------------------------------------------------------
-- |
-- Module    : Data.Ruby.Marshal.Int
-- Copyright : (c) Philip Cunningham, 2015
-- License   : MIT
--
-- Maintainer:  802389+philipcunningham@users.noreply.github.com
-- Stability :  experimental
-- Portability: portable
--
-- Parsers for signed and unsigned integrals.
--
--------------------------------------------------------------------

module Data.Ruby.Marshal.Int (
  -- * Signed integrals
    getInt8
  , getInt16le
  , getInt24le
  , getInt32le
  , Int16
  -- * Unsigned integrals
  , getWord8
  , getWord16le
  , getWord24le
  , getWord32le
  , Word8
) where

import           Control.Applicative
import           Data.Bits ((.|.), shiftL)
import qualified Data.ByteString as BS
import           Data.Int (Int8, Int16, Int32)
import           Data.Serialize.Get (Get, getBytes, getWord8, getWord16le, getWord32le)
import           Data.Word (Word8, Word32)
import           Prelude

-- | Read an Int8.
getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8

-- | Read an Int16.
getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16le

-- | Read a Word24 in little endian format. Since Word24 unavailable in Data.Int
-- we use Word32.
getWord24le :: Get Word32
getWord24le = do
  s <- getBytes 3
  return $! (fromIntegral (s `BS.index` 2) `shiftL` 16) .|.
            (fromIntegral (s `BS.index` 1) `shiftL` 8) .|.
             fromIntegral (s `BS.index` 0)

-- | Read an Int24. Since Int24 unavailable in Data.Int we use Int32.
getInt24le :: Get Int32
getInt24le = fromIntegral <$> getWord24le

-- | Read an Int32.
getInt32le :: Get Int32
getInt32le = fromIntegral <$> getWord32le

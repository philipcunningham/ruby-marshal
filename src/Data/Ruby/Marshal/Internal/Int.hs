{-# LANGUAGE MultiWayIf #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.Ruby.Marshal.Internal.Int
-- Copyright : (c) Philip Cunningham, 2015
-- License   : MIT
--
-- Maintainer:  802389+philipcunningham@users.noreply.github.com
-- Stability :  experimental
-- Portability: portable
--
-- Helper module for parsing Int.
--
--------------------------------------------------------------------

module Data.Ruby.Marshal.Internal.Int (
  getWord8,
  getInt8,
  getWord16le,
  getInt16le,
  getWord24le,
  getInt24le,
  getWord32le,
  getInt32le,
  Int16,
  Word8
) where

import Control.Applicative

import Data.Bits          ((.|.), shiftL)
import Data.Int           (Int8, Int16, Int32)
import Data.Serialize.Get (Get, getBytes, getWord8, getWord16le, getWord32le)
import Data.Word          (Word8, Word32)

import qualified Data.ByteString as BS

getInt8 :: Get Int8
getInt8 = fromIntegral <$> getWord8

getInt16le :: Get Int16
getInt16le = fromIntegral <$> getWord16le

getWord24le :: Get Word32
getWord24le = do
  s <- getBytes 3
  return $! (fromIntegral (s `BS.index` 2) `shiftL` 16) .|.
            (fromIntegral (s `BS.index` 1) `shiftL`  8) .|.
             fromIntegral (s `BS.index` 0)

getInt24le :: Get Int32
getInt24le = fromIntegral <$> getWord24le

getInt32le :: Get Int32
getInt32le = fromIntegral <$> getWord32le

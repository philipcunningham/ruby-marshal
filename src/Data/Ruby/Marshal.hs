{-# LANGUAGE OverloadedStrings #-}

module Data.Ruby.Marshal (
  loadVerbose, load,
  module Data.Ruby.Marshal.Get,
  module Data.Ruby.Marshal.Object
) where

import Data.Ruby.Marshal.Get
import Data.Ruby.Marshal.Object

import Data.Serialize.Get (runGet, getWord8)
import Data.Word          (Word8)

import qualified Data.ByteString as BS

loadVerbose :: BS.ByteString -> Either String ((Word8, Word8), RubyObject)
loadVerbose =
  runGet $ do
    major  <- getWord8
    minor  <- getWord8
    object <- getRubyObject
    return ((major, minor), object)

load :: BS.ByteString -> Either String RubyObject
load x = case loadVerbose x of
   (Right ((_, _), y)) -> Right y
   Left z              -> Left  z

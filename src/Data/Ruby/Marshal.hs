{-# LANGUAGE OverloadedStrings #-}

module Data.Ruby.Marshal (
  loadVerbose, load,
  module Data.Ruby.Marshal.Get,
  module Data.Ruby.Marshal.Object
) where

import Data.Serialize.Get       (runGet)
import Data.Ruby.Marshal.Object (RubyObject(..), getRubyObject)
import Data.Ruby.Marshal.Get    (getUnsignedInt)

import qualified Data.ByteString as BS

loadVerbose :: BS.ByteString -> Either String ((Int, Int), RubyObject)
loadVerbose =
  runGet f
  where
    f = do
      major  <- getUnsignedInt
      minor  <- getUnsignedInt
      object <- getRubyObject
      return ((major, minor), object)

load :: BS.ByteString -> Either String RubyObject
load x = case loadVerbose x of
   (Right ((_, _), y)) -> Right y
   Left z              -> Left  z

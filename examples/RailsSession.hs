{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Ruby.Marshal          (decode, RubyObject(..))
import Data.Ruby.Marshal.Encoding (REncoding(..))
import Data.Ruby.Marshal.Types    (fromRuby)
import System.Directory           (getCurrentDirectory)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as DM

lookupUserID :: (BS.ByteString, REncoding)
             -> RubyObject
             -> Maybe (BS.ByteString, REncoding)
lookupUserID key hash = fromRuby hash >>= \map' ->
  DM.lookup key map'

main :: IO ()
main = do
  dir <- getCurrentDirectory
  rbs <- BS.readFile (mconcat [dir, "/test/bin/railsCookie.bin"])
  print $ case decode rbs of
    Just cookie -> lookupUserID ("user_id", UTF_8) cookie
    Nothing     -> Nothing

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString   (ByteString)
import qualified Data.ByteString   as BS
import qualified Data.Map.Strict   as DM
import           Data.Ruby.Marshal
import           System.Directory  (getCurrentDirectory)

lookupUserID :: (ByteString, RubyStringEncoding)
             -> RubyObject
             -> Maybe (ByteString, RubyStringEncoding)
lookupUserID key hash = fromRuby hash >>= \cookie -> DM.lookup key cookie

main :: IO ()
main = do
  dir <- getCurrentDirectory
  rbs <- BS.readFile (mconcat [dir, "/test/bin/railsCookie.bin"])
  print $
    case decode rbs of
      Just cookie -> lookupUserID ("user_id", UTF_8) cookie
      Nothing     -> Nothing

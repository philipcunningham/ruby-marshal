{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Ruby.Marshal
import System.Directory  (getCurrentDirectory)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as DM

lookupUserID :: IVar -> RubyObject -> Maybe IVar
lookupUserID key hash = fromRuby hash >>= \map' ->
  DM.lookup key map'

main :: IO ()
main = do
  dir <- getCurrentDirectory
  rbs <- BS.readFile (mconcat [dir, "/test/bin/railsCookie.bin"])
  print $ case decode rbs of
    Just cookie -> lookupUserID ("user_id", UTF_8) cookie
    Nothing     -> Nothing

{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Monoid       (mconcat)
import Data.Ruby.Marshal (decode, RubyObject(..))
import System.Directory  (getCurrentDirectory)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as DM

key :: RubyObject
key = RIVar (RString "user_id", "UTF-8")

lookupId :: RubyObject -> Maybe RubyObject
lookupId (RHash cookie) = DM.lookup key cookie
lookupId _              = Nothing

main :: IO ()
main = do
  dir <- getCurrentDirectory
  rbs <- BS.readFile (mconcat [dir, "/test/bin/railsCookie.bin"])
  print $ case decode rbs of
    Just cookie -> lookupId cookie
    Nothing     -> Nothing

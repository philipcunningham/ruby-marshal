{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Monoid      (mconcat)
import System.Directory (getCurrentDirectory)

import Data.Ruby.Marshal (load, RubyObject(..))

import qualified Data.ByteString as BS

main :: IO ()
main = do
  dir <- getCurrentDirectory
  rbs <- BS.readFile (mconcat [dir, "/test/bin/bigArray.bin"])
  putStrLn . show $ load rbs >>= \case
    RArray xs -> Just $ xs
    _         -> Nothing

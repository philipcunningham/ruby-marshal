{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Monoid      (mconcat)
import System.Directory (getCurrentDirectory)

import Data.Ruby.Marshal (load, RubyObject(..))

import qualified Data.ByteString as BS

main :: IO ()
main = do
  dir <- getCurrentDirectory
  rbs <- BS.readFile (mconcat [dir, "/test/bin/42.bin"])
  putStrLn . show $ load rbs >>= \case
    RFixnum x -> Just (x * x)
    _         -> Nothing

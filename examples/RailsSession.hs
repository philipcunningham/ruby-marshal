module Main where

import Data.Monoid       (mconcat)
import Data.Ruby.Marshal (decode)
import System.Directory  (getCurrentDirectory)

import qualified Data.ByteString as BS

main :: IO ()
main = do
  dir <- getCurrentDirectory
  rbs <- BS.readFile (mconcat [dir, "/test/bin/railsCookie.bin"])
  print $ decode rbs

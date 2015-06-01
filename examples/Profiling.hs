module Main where

import Data.Monoid      (mconcat)
import System.Directory (getCurrentDirectory)

import qualified Data.ByteString   as BS
import qualified Data.Ruby.Marshal as Marshal

main :: IO ()
main = do
  dir <- getCurrentDirectory
  rbs <- BS.readFile (mconcat [dir, "/test/bin/42.bin"])
  putStrLn . show $ Marshal.load rbs

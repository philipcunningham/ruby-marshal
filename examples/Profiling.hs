{-# LANGUAGE LambdaCase #-}

module Main where

import qualified Data.ByteString   as BS
import qualified Data.Foldable     as F
import           Data.Monoid       (mconcat)
import           Data.Ruby.Marshal (RubyObject (..), decode)
import           Data.Vector       (Vector)
import           System.Directory  (getCurrentDirectory)

sumFixnum :: Vector RubyObject -> Int
sumFixnum xs = F.foldl' (+) 0 $ fmap f xs
  where
    f :: RubyObject -> Int
    f (RFixnum x) = x

main :: IO ()
main = do
  dir <- getCurrentDirectory
  rbs <- BS.readFile (mconcat [dir, "/test/bin/bigArray.bin"])
  print $ decode rbs >>= \case
            RArray xs -> Just $ sumFixnum xs
            _         -> Nothing

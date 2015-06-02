{-# LANGUAGE LambdaCase #-}

module Main where

import Data.Monoid      (mconcat)
import System.Directory (getCurrentDirectory)

import Data.Ruby.Marshal (load, RubyObject(..))
import Data.Vector       (Vector)
import Debug.Trace       (traceShow)

import qualified Data.ByteString as BS
import qualified Data.Foldable   as F

debug :: Show a => a -> a
debug x = traceShow x x

loadBigArray :: IO (Maybe RubyObject)
loadBigArray = do
  dir <- getCurrentDirectory
  rbs <- BS.readFile (mconcat [dir, "/test/bin/bigArray.bin"])
  return $ load rbs

sumFixnum :: Vector RubyObject -> Integer
sumFixnum xs = F.foldr' (+) 0 $ (debug (fmap f xs))
  where
    f :: RubyObject -> Integer
    f (RFixnum x) = toInteger x

main :: IO ()
main = do
  array <- loadBigArray
  print $ case array of
    Just (RArray xs) -> Just $ sumFixnum xs
    _                -> Nothing

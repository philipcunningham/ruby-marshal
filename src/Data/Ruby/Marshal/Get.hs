{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ruby.Marshal.Get (
  getNil, getBool, getFixnum, getArray, getHash, getString, getFloat
) where

import Control.Applicative

import Control.Monad      (guard)
import Data.Bits          ((.&.), (.|.), complement, shiftL)
import Data.Serialize.Get (Get, getBytes, getWord8)
import Data.String.Conv   (toS)
import Text.Read          (readMaybe)
import Data.Word          (Word8)
import Prelude

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

getNil :: Get ()
getNil = tag 48

getBool :: Get Bool
getBool = True <$ tag 84 <|> False <$ tag 70

getFixnum :: Get Int
getFixnum = zero <|> between5and127 <|> betweenNeg128andNeg3 <|> greaterThan122 <|> lessThanNeg123
  where
    -- 0
    zero :: Get Int
    zero = 0 <$ tag 0
    -- 5 && 127
    between5and127 :: Get Int
    between5and127 = do
      x <- getSignedInt
      if | x > 4 && x < 128 -> return (x - 5)
         | otherwise        -> empty
    -- -128 && -3
    betweenNeg128andNeg3 :: Get Int
    betweenNeg128andNeg3 = do
      x <- getSignedInt
      if | x > -129 && x < -4 -> return (x + 5)
         | otherwise          -> empty
    -- (> 122)
    greaterThan122 :: Get Int
    greaterThan122 = do
      x <- getSignedInt
      if x < 0 then empty else
       for (return 0) 0 (< x) (+ 1) $ twiddleWith f
      where
        f :: Int -> Int -> Int -> Int
        f x' y' z' = x' .|. (y' `shiftL` (8 * z'))
    -- (< -123)
    lessThanNeg123 :: Get Int
    lessThanNeg123 = do
      x <- getSignedInt
      for (return (-1)) 0 (< (-x)) (+ 1) $ twiddleWith f
      where
        f :: Int -> Int -> Int -> Int
        f x' y' z' = (x' .&. complement (255 `shiftL` (8 * z'))) .|. (y' `shiftL` (8 * z'))

getArray :: Get a -> Get (V.Vector a)
getArray g = getFixnum >>= \len -> V.replicateM len g

getHash :: Get a -> Get b -> Get (V.Vector (a, b))
getHash k v = getFixnum >>= \len -> V.replicateM len $ (,) <$> k <*> v

getString :: Get BS.ByteString
getString = getFixnum >>= getBytes

getFloat :: Get Double
getFloat = getFixnum >>= getBytes >>= \str ->
  case readMaybe . toS $ str of
    Just x  -> return x
    Nothing -> empty

getUnsignedInt :: Get Int
getUnsignedInt = getWord8 >>= \c -> return $ fromEnum c

getSignedInt :: Get Int
getSignedInt = getUnsignedInt >>= \i -> return $ if i > 127 then i - 256 else i

tag :: Word8 -> Get ()
tag t = getWord8 >>= \b -> guard $ t == b

twiddleWith :: (Int -> Int -> Int -> Int) -> (Get Int, Int) -> Get Int
twiddleWith f (acc, index) = do
  x <- acc
  y <- getUnsignedInt
  return $ f x y index

for :: a -> b -> (b -> Bool) -> (b -> b) -> ((a, b) -> a) -> a
for acc index predicate modifier body =
  if predicate index then
    for (body (acc, index)) (modifier $! index) predicate modifier body
  else acc

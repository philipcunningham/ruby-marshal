{-# LANGUAGE MultiWayIf #-}

module Data.Ruby.Marshal.Get (
  getNil, getBool, getUnsignedInt, getFixnum, getArray, getHash
) where

import Control.Applicative

import Control.Monad       (guard)
import Data.Serialize.Get  (Get, getWord8, skip)
import Data.Bits           ((.&.), (.|.), complement, shiftL)
import Data.Word           (Word8)
import Prelude

import qualified Data.Vector as V

for :: a -> b -> (b -> Bool) -> (b -> b) -> ((a, b) -> a) -> a
for acc index predicate modifier body =
  if predicate index then
    for (body (acc, index)) (modifier $! index) predicate modifier body
  else acc

tag :: Word8 -> Get ()
tag t = do
  b <- getWord8
  guard $ t == b

getUnsignedInt :: Get Int
getUnsignedInt = do
  w <- getWord8
  return $ fromEnum w

getSignedInt :: Get Int
getSignedInt = do
  i <- getUnsignedInt
  return $ if i > 127 then i - 256 else i

getNil :: Get ()
getNil = tag 48

getBool :: Get Bool
getBool = True <$ tag 84 <|> False <$ tag 70

getZero :: Get Int
getZero = 0 <$ tag 0

getBetween5and127 :: Get Int
getBetween5and127 = do
  x <- getSignedInt
  if | x > 4 && x < 128 -> return (x - 5)
     | otherwise        -> empty

getBetweenNeg128andNeg3 :: Get Int
getBetweenNeg128andNeg3 = do
  x <- getSignedInt
  if | x > -129 && x < -4 -> return (x + 5)
     | otherwise          -> empty

twiddleWith :: (Int -> Int -> Int -> Int) -> (Get Int, Int) -> Get Int
twiddleWith f (acc, index) = do
  x <- acc
  y <- getUnsignedInt
  return $ f x y index

getGreaterThan122 :: Get Int
getGreaterThan122 = do
  x <- getSignedInt
  if x < 0 then empty else
   for (return 0) 0 (< x) (+ 1) $ twiddleWith f
  where
    f :: Int -> Int -> Int -> Int
    f x' y' z' = x' .|. (y' `shiftL` (8 * z'))

getLessThanNeg123 :: Get Int
getLessThanNeg123 = do
  x <- getSignedInt
  for (return (-1)) 0 (< (-x)) (+ 1) $ twiddleWith f
  where
    f :: Int -> Int -> Int -> Int
    f x' y' z' = (x' .&. complement (255 `shiftL` (8 * z'))) .|. (y' `shiftL` (8 * z'))

getFixnum :: Get Int
getFixnum = do
  _ <- skip 1
  getZero <|> getBetween5and127 <|> getBetweenNeg128andNeg3
          <|> getGreaterThan122 <|> getLessThanNeg123

getArray :: Get a -> Get (V.Vector a)
getArray g = do
  len <- getFixnum
  V.replicateM len g

getHash :: Get a -> Get b -> Get (V.Vector (a, b))
getHash k v = do
  len <- getFixnum
  V.replicateM len $ (,) <$> k <*> v

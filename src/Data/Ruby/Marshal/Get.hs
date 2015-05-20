{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Ruby.Marshal.Get (
  getNil, getBool, getFixnum, getArray, getHash, getString, getFloat
) where

import Control.Applicative

import Control.Monad      (guard)
import Data.Bits          ((.&.), (.|.), complement, shiftL)
import Data.Serialize.Get (Get, getBytes, getTwoOf, getWord8)
import Data.String.Conv   (toS)
import Data.Word          (Word8)
import Text.Read          (readMaybe)
import Prelude

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

getNil :: Get ()
getNil = tag 48

getBool :: Get Bool
getBool = True <$ tag 84 <|> False <$ tag 70

getFixnum :: Get Int
getFixnum = zero <|> bt0and122 <|> btNeg123and2 <|> gt122 <|> ltNeg123
  where
    -- 0.
    zero :: Get Int
    zero = 0 <$ tag 0
    -- between 0 and 122.
    bt0and122 :: Get Int
    bt0and122 = do
      x <- getSignedInt
      if | x >= 5 && x <= 127 -> return (x - 5)
         | otherwise          -> empty
    -- between -123 and 2.
    btNeg123and2 :: Get Int
    btNeg123and2 = do
      x <- getSignedInt
      if | x >= -128 && x <= -3 -> return (x + 5)
         | otherwise            -> empty
    -- greater than 122.
    gt122 :: Get Int
    gt122 = do
      x <- getSignedInt
      if x < 0 then empty else
       for (return 0) 0 (< x) (+ 1) $ twiddle f
      where
        f :: Int -> Int -> Int -> Int
        f x' y' z' = x' .|. (y' `shiftL` (8 * z'))
    -- less than -123.
    ltNeg123 :: Get Int
    ltNeg123 = do
      x <- getSignedInt
      for (return (-1)) 0 (< (-x)) (+ 1) $ twiddle f
      where
        f :: Int -> Int -> Int -> Int
        f x' y' z' = (x' .&. complement (255 `shiftL` (8 * z'))) .|. (y' `shiftL` (8 * z'))

getArray :: Get a -> Get (V.Vector a)
getArray g = getFixnum >>= \len -> V.replicateM len g

getHash :: Get a -> Get b -> Get (V.Vector (a, b))
getHash k v = getFixnum >>= \len -> V.replicateM len $ getTwoOf k v

getString :: Get a -> Get BS.ByteString
getString g = getByteString <* getEncoding
  where getEncoding = getWord8 >> getWord8 >> getByteString >> g

getFloat :: Get Double
getFloat = getByteString >>= \x ->
  case readMaybe . toS $ x of
    Just y  -> return y
    Nothing -> empty

getByteString :: Get BS.ByteString
getByteString = getFixnum >>= getBytes

getUnsignedInt :: Get Int
getUnsignedInt = getWord8 >>= return . fromEnum

getSignedInt :: Get Int
getSignedInt = getUnsignedInt >>= \x -> return $ if x > 127 then x - 256 else x

tag :: Word8 -> Get ()
tag t = getWord8 >>= \b -> guard $ t == b

twiddle :: (Int -> Int -> Int -> Int) -> (Get Int, Int) -> Get Int
twiddle f (acc, i) = acc >>= \x -> getUnsignedInt >>= \y -> return $ f x y i

for :: a -> b -> (b -> Bool) -> (b -> b) -> ((a, b) -> a) -> a
for acc index predicate modifier body =
  if predicate index then
    for (body (acc, index)) (modifier $! index) predicate modifier body
  else acc

{-# LANGUAGE MultiWayIf #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.Ruby.Marshal.Get
-- Copyright : (c) Philip Cunningham, 2015
-- License   : MIT
--
-- Maintainer:  802389+philipcunningham@users.noreply.github.com
-- Stability :  experimental
-- Portability: portable
--
-- Ruby Marshal deserialiser using @Data.Serialize@.
--
--------------------------------------------------------------------

module Data.Ruby.Marshal.Get (
  getNil, getBool, getFixnum, getArray, getHash, getString, getFloat
) where

import Control.Applicative

import Control.Monad      (guard, liftM)
import Data.Bits          ((.&.), (.|.), complement, shiftL)
import Data.Serialize.Get (Get, getBytes, getTwoOf, getWord8, label)
import Data.String.Conv   (toS)
import Data.Word          (Word8)
import Text.Read          (readMaybe)
import Prelude

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

-- | Deserialises <http://ruby-doc.org/core-2.2.0/NilClass.html nil>.
getNil :: Get ()
getNil = label "Nil" $ tag 48

-- | Deserialises <http://ruby-doc.org/core-2.2.0/TrueClass.html true> and
-- <http://ruby-doc.org/core-2.2.0/FalseClass.html false>.
getBool :: Get Bool
getBool = label "Bool" $
  True <$ tag 84 <|> False <$ tag 70

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Fixnum.html Fixnum>.
getFixnum :: Get Int
getFixnum = label "Fixnum" $
  zero <|> bt0and122 <|> btNeg123and2 <|> gt122 <|> ltNeg123
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

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Array.html Array>.
getArray :: Get a -> Get (V.Vector a)
getArray g = label "Array" $
  getFixnum >>= \len -> V.replicateM len g

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Hash.html Hash>.
getHash :: Get a -> Get b -> Get (V.Vector (a, b))
getHash k v = label "Hash" $
  getFixnum >>= \len -> V.replicateM len $ getTwoOf k v

-- | Deserialises <http://ruby-doc.org/core-2.2.0/String.html String>.
getString :: Get a -> Get BS.ByteString
getString g = label "String" $
  getRawString <* getEncoding -- For now we just throw away the encoding info.
  where getEncoding = getWord8 >> getWord8 >> getRawString >> g

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Float.html Float>.
getFloat :: Get Double
getFloat = label "Float" $ getRawString >>= \x ->
  case readMaybe . toS $ x of
    Just y  -> return y
    Nothing -> empty

getRawString :: Get BS.ByteString
getRawString = label "RawString" $
  getFixnum >>= getBytes

getSignedInt :: Get Int
getSignedInt = label "SignedInt" $
  getUnsignedInt >>= \x -> return $ if x > 127 then x - 256 else x

getUnsignedInt :: Get Int
getUnsignedInt = label "UnsignedInt" $
  liftM fromEnum getWord8

for :: a -> b -> (b -> Bool) -> (b -> b) -> ((a, b) -> a) -> a
for acc index predicate modifier body =
  if predicate index then
    for (body (acc, index)) (modifier $! index) predicate modifier body
  else acc

tag :: Word8 -> Get ()
tag t = label "Tag" $
  getWord8 >>= \b -> guard $ t == b

twiddle :: (Int -> Int -> Int -> Int) -> (Get Int, Int) -> Get Int
twiddle f (acc, i) = acc >>= \x -> getUnsignedInt >>= \y -> return $ f x y i

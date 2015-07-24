{-# LANGUAGE BangPatterns #-}
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
  getMarshalVersion,
  getNil,
  getBool,
  getFixnum,
  getArray,
  getHash,
  getString,
  getFloat,
  getSymbol,
  getSymlink
) where

import Control.Applicative
import Data.Ruby.Marshal.Internal.Int
import Prelude

import Data.Ruby.Marshal.Types
-- import Control.Monad      (guard)
import Control.Monad.State
import Data.Serialize.Get (Get, getBytes, getTwoOf, label)
import Data.String.Conv   (toS)
import Text.Read          (readMaybe)

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

-- | Deserialises Marshal version.
getMarshalVersion :: Marshal (Word8, Word8)
getMarshalVersion = liftMarshal (label "Marshal Version" $ (getTwoOf getWord8 getWord8))

-- | Deserialises <http://ruby-doc.org/core-2.2.0/NilClass.html nil>.
getNil :: Marshal ()
getNil = liftMarshal $ label "Nil" $ tag 48

-- | Deserialises <http://ruby-doc.org/core-2.2.0/TrueClass.html true> and
-- <http://ruby-doc.org/core-2.2.0/FalseClass.html false>.
getBool :: Marshal Bool
getBool = liftMarshal $ label "Bool" $
  True <$ tag 84 <|> False <$ tag 70

-- === Fixnum and long

-- "i" represents a signed 32 bit value using a packed format.  One through five
-- bytes follows the type.  The value loaded will always be a Fixnum.  On
-- 32 bit platforms (where the precision of a Fixnum is less than 32 bits)
-- loading large values will cause overflow on CRuby.

-- The fixnum type is used to represent both ruby Fixnum objects and the sizes of
-- marshaled arrays, hashes, instance variables and other types.  In the
-- following sections "long" will mean the format described below, which supports
-- full 32 bit precision.

-- The first byte has the following special values:

-- "\x00"::
--   The value of the integer is 0.  No bytes follow.

-- "\x01"::
--   The total size of the integer is two bytes.  The following byte is a
--   positive integer in the range of 0 through 255.  Only values between 123
--   and 255 should be represented this way to save bytes.

-- "\xff"::
--   The total size of the integer is two bytes.  The following byte is a
--   negative integer in the range of -1 through -256.

-- "\x02"::
--   The total size of the integer is three bytes.  The following two bytes are a
--   positive little-endian integer.

-- "\xfe"::
--   The total size of the integer is three bytes.  The following two bytes are a
--   negative little-endian integer.

-- "\x03"::
--   The total size of the integer is four bytes.  The following three bytes are
--   a positive little-endian integer.

-- "\xfd"::
--   The total size of the integer is two bytes.  The following three bytes are a
--   negative little-endian integer.

-- "\x04"::
--   The total size of the integer is five bytes.  The following four bytes are a
--   positive little-endian integer.  For compatibility with 32 bit ruby,
--   only Fixnums less than 1073741824 should be represented this way.  For sizes
--   of stream objects full precision may be used.

-- "\xfc"::
--   The total size of the integer is two bytes.  The following four bytes are a
--   negative little-endian integer.  For compatibility with 32 bit ruby,
--   only Fixnums greater than -10737341824 should be represented this way.  For
--   sizes of stream objects full precision may be used.

-- Otherwise the first byte is a sign-extended eight-bit value with an offset.
-- If the value is positive the value is determined by subtracting 5 from the
-- value.  If the value is negative the value is determined by adding 5 to the
-- value.

-- There are multiple representations for many values.  CRuby always outputs the
-- shortest representation possible.

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Fixnum.html Fixnum>.
getFixnum :: Marshal Int
getFixnum = liftMarshal $ label "Fixnum" $ do
  x <- getInt8
  if | x ==  0   -> fromIntegral <$> return x
     | x ==  1   -> fromIntegral <$> getWord8
     | x == -1   -> fromIntegral <$> getNegInt16
     | x ==  2   -> fromIntegral <$> getWord16le
     | x == -2   -> fromIntegral <$> getInt16le
     | x ==  3   -> fromIntegral <$> getWord24le
     | x == -3   -> fromIntegral <$> getInt24le
     | x ==  4   -> fromIntegral <$> getWord32le
     | x == -4   -> fromIntegral <$> getInt32le
     | x >=  6   -> fromIntegral <$> return (x - 5)
     | x <= -6   -> fromIntegral <$> return (x + 5)
     | otherwise -> empty
  where
    getNegInt16 :: Get Int16
    getNegInt16 =  do
      x <- fromIntegral <$> getInt8
      if x >= 0 && x <= 127 then return (x - 256) else return x

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Array.html Array>.
getArray :: Marshal a -> Marshal (V.Vector a)
getArray g = do
  len <- getFixnum
  v <- getVec len g (return V.empty)
  liftMarshal $ label "Array" $ return v

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Hash.html Hash>.
getHash :: Marshal a -> Marshal b -> Marshal (V.Vector (a, b))
getHash k v = do
  r <- getFixnum >>= \len -> V.replicateM len (liftM2 (,) k v)
  liftMarshal (label "Hash" $ return r)

-- | Deserialises <http://ruby-doc.org/core-2.2.0/String.html String>.
getString :: Marshal a -> Marshal BS.ByteString
getString g = do
  r <- getRawString <* getEncoding -- For now we just throw away the encoding info.
  liftMarshal (label "String" $ return r)
  where
    getEncoding = do
      _ <- liftMarshal $ (getWord8 >> getWord8)
      _ <- getRawString
      g

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Float.html Float>.
getFloat :: Marshal Double
getFloat = do
  r <- getRawString >>= \x ->
    case readMaybe . toS $ x of
      Just y  -> return y
      Nothing -> liftMarshal empty
  liftMarshal (label "Float" $ return r)

-- | Deserialises <http://ruby-doc.org/core-2.2.0/Symbol.html Symbol>.
getSymbol :: Marshal BS.ByteString
getSymbol = do
  r <- getRawString
  cache (RSymbol r)
  liftMarshal $ label "Symbol" $ return r

-- TODO: We should patter match on this and insert
-- appropriately Objects or symbols.
cache :: RubyObject -> Marshal ()
cache obj = do
  c <- get
  let oldS = symbols c
  let newC = c { symbols = V.snoc oldS obj }
  put newC

lookupSym :: Int -> Marshal (Maybe RubyObject)
lookupSym i = do
  c <- gets symbols
  return $ c V.!? i

getSymlink :: Marshal BS.ByteString
getSymlink = do
  i <- getFixnum
  maybeObject <- lookupSym i
  case maybeObject of
    Just (RSymbol bs) -> return bs
    _                 -> fail "getSymlink "

-- | Gets a Vector of n elements.
getVec :: Int -> Marshal a -> Marshal (V.Vector a) -> Marshal (V.Vector a)
getVec !n !x !y =
  if | n == 0    -> y
     | otherwise -> do
         x' <- x
         y' <- y
         getVec (n - 1) x (return $! V.snoc y' x')

-- | Gets a raw string.
getRawString :: Marshal BS.ByteString
getRawString = do
  r <- getFixnum >>= (\i -> (liftMarshal $ getBytes i))
  liftMarshal (label "RawString" $ return r)

-- | Guard against invalid input.
tag :: Word8 -> Get ()
tag t = label "Tag" $
  getWord8 >>= \b -> guard $ t == b

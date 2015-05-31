{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.Ruby.Marshal.Object
-- Copyright : (c) Philip Cunningham, 2015
-- License   : MIT
--
-- Maintainer:  802389+philipcunningham@users.noreply.github.com
-- Stability :  experimental
-- Portability: portable
--
-- RubyObject definition.
--
--------------------------------------------------------------------

module Data.Ruby.Marshal.Object (
  RubyObject(..)
) where

import Control.Applicative
import Data.Ruby.Marshal.Get
import Prelude

import Data.Serialize      (Serialize(..))
import Data.Serialize.Get  (Get, getWord8)
import Data.Vector         (Vector)
import Data.Word           (Word8)

import qualified Data.ByteString as BS

data Error
  = Unknown
    -- ^ represents an unknown Ruby object
  | Unsupported
    -- ^ represents an unsupported Ruby object
  deriving (Eq, Show)

data RubyObject
  = RNil
    -- ^ represents @nil@
  | RBool                  !Bool
    -- ^ represents @true@ or @false@
  | RFixnum {-# UNPACK #-} !Int
    -- ^ represents a @Fixnum@
  | RArray                 !(Vector RubyObject)
    -- ^ represents an @Array@
  | RHash                  !(Vector (RubyObject, RubyObject))
    -- ^ represents an @Hash@
  | RString                !BS.ByteString
    -- ^ represents a @String@
  | RFloat {-# UNPACK #-}  !Double
    -- ^ represents a @Float@
  | RError                 !Error
    -- ^ represents an invalid object
  deriving (Eq, Show)

pattern NilC    = 48
pattern TrueC   = 84
pattern FalseC  = 70
pattern FixnumC = 105
pattern ArrayC  = 91
pattern HashC   = 123
pattern IvarC   = 73
pattern StringC = 34
pattern FloatC  = 102

-- | Parses Marshal version.
getMarshalVersion :: Get (Word8, Word8)
getMarshalVersion = getWord8 >>= \x -> getWord8 >>= \y -> return (x, y)

-- | Parses a subset of Ruby objects.
getRubyObject :: Get RubyObject
getRubyObject = getMarshalVersion >> getRuby
  where
    getRuby :: Get RubyObject
    getRuby = getWord8 >>= \case
      NilC    -> return RNil
      TrueC   -> return $ RBool True
      FalseC  -> return $ RBool False
      FixnumC -> RFixnum <$> getFixnum
      ArrayC  -> RArray  <$> getArray getRuby
      HashC   -> RHash   <$> getHash getRuby getRuby
      IvarC   -> getWord8 >>= \case StringC -> RString <$> getString getRuby
                                    _       -> return $ RError Unsupported
      FloatC  -> RFloat <$> getFloat
      _       -> return $ RError Unsupported

instance Serialize RubyObject where
  get = getRubyObject
  put = error "unsupported operation"

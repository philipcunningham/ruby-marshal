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
  RubyObject(..),
  getRubyObject
) where

import Control.Applicative
import Data.Ruby.Marshal.Get
import Prelude

import Data.Serialize.Get  (Get, getWord8)
import Data.Vector         (Vector)

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
  | RBool                       !Bool
    -- ^ represents @true@ or @false@
  | RFixnum      {-# UNPACK #-} !Int
    -- ^ represents a @Fixnum@
  | RArray                      !(Vector RubyObject)
    -- ^ represents an @Array@
  | RHash                       !(Vector (RubyObject, RubyObject))
    -- ^ represents an @Hash@
  | RString                     !BS.ByteString
    -- ^ represents a @String@
  | RFloat                      !Double
    -- ^ represents a @Float@
  | RError                      !Error
    -- ^ represents an invalid object
  deriving (Eq, Show)

pattern CNil    = 48
pattern CTrue   = 84
pattern CFalse  = 70
pattern CFixnum = 105
pattern CArray  = 91
pattern CHash   = 123
pattern CIvar   = 73
pattern CString = 34
pattern CFloat  = 102

-- | Parses a subset of Ruby objects.
getRubyObject :: Get RubyObject
getRubyObject = getWord8 >>= \case
  CNil    -> return RNil
  CTrue   -> return $ RBool True
  CFalse  -> return $ RBool False
  CFixnum -> RFixnum <$> getFixnum
  CArray  -> RArray  <$> getArray getRubyObject
  CHash   -> RHash   <$> getHash getRubyObject getRubyObject
  CIvar   -> getWord8 >>= \case CString -> RString <$> getString getRubyObject
                                _       -> unsupported
  CFloat  -> RFloat <$> getFloat
  _       -> unsupported
  where
    unsupported = return $ RError Unsupported

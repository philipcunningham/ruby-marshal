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
  getRubyObject,
  RubyObject(..)
) where

import Control.Applicative
import Data.Ruby.Marshal.Get
import Data.Ruby.Marshal.Types
import Prelude

import Data.Serialize.Get (Get, getWord8)

-- | Parses a subset of Ruby objects.
getRubyObject :: Get RubyObject
getRubyObject = getMarshalVersion >> go
  where
    go :: Get RubyObject
    go = getWord8 >>= \case
      NilC    -> return RNil
      TrueC   -> return $ RBool True
      FalseC  -> return $ RBool False
      FixnumC -> RFixnum <$> getFixnum
      ArrayC  -> RArray  <$> getArray go
      HashC   -> RHash   <$> getHash go go
      IvarC   -> getWord8 >>= \case StringC -> RString <$> getString go
                                    _       -> return $ RError Unsupported
      FloatC  -> RFloat <$> getFloat
      SymbolC -> RSymbol <$> getSymbol
      _       -> return $ RError Unsupported

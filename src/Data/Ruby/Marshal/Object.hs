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
  emptyCache,
  getRubyObject,
  RubyObject(..),
  Marshal(..)
) where

import Control.Applicative
import Data.Ruby.Marshal.Get
import Data.Ruby.Marshal.Types
import Prelude

import Data.Serialize.Get (getWord8)

import qualified Data.Vector as V

-- | Constructs an empty cache to store symbols and objects.
emptyCache :: Cache
emptyCache = Cache { symbols = V.empty, objects = V.empty }

-- | Parses a subset of Ruby objects.
getRubyObject :: Marshal RubyObject
getRubyObject = getMarshalVersion >> go
  where
    go :: Marshal RubyObject
    go = liftMarshal getWord8 >>= \case
      NilC        -> return RNil
      TrueC       -> return $ RBool True
      FalseC      -> return $ RBool False
      ArrayC      -> RArray  <$> getArray go
      FixnumC     -> RFixnum <$> getFixnum
      FloatC      -> RFloat  <$> getFloat
      HashC       -> RHash   <$> getHash go go
      IvarC       -> RIvar   <$> getIvar go
      ObjectLinkC -> RIvar   <$> getObjectLink
      StringC     -> RString <$> getString
      SymbolC     -> RSymbol <$> getSymbol
      SymlinkC    -> RSymbol <$> getSymlink
      _           -> return $ RError Unsupported

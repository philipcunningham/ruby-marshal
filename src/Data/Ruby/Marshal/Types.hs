{-# LANGUAGE PatternSynonyms #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.Ruby.Marshal.Types
-- Copyright : (c) Philip Cunningham, 2015
-- License   : MIT
--
-- Maintainer:  802389+philipcunningham@users.noreply.github.com
-- Stability :  experimental
-- Portability: portable
--
-- Common types for Ruby Marshal deserialisation.
--
--------------------------------------------------------------------

module Data.Ruby.Marshal.Types (
  -- * Marshal Monad
    Marshal
  -- * Internal cache
  , Cache
  -- * Ruby string encodings
  , RubyStringEncoding(..)
  -- * Ruby object
  , RubyObject(..)
  -- * Patterns
  , pattern NilChar
  , pattern FalseChar
  , pattern TrueChar
  , pattern ArrayChar
  , pattern FixnumChar
  , pattern FloatChar
  , pattern HashChar
  , pattern IVarChar
  , pattern ObjectLinkChar
  , pattern StringChar
  , pattern SymbolChar
  , pattern SymlinkChar
) where

import Data.Ruby.Marshal.Encoding
import Data.Ruby.Marshal.Monad
import Data.Ruby.Marshal.RubyObject

-- | Character that represents NilCharlass.
pattern NilChar = 48
-- | Character that represents FalseClass.
pattern FalseChar = 70
-- | Character that represents TrueClass.
pattern TrueChar = 84
-- | Character that represents Array.
pattern ArrayChar = 91
-- | Character that represents Fixnum.
pattern FixnumChar = 105
-- | Character that represents Float.
pattern FloatChar = 102
-- | Character that represents Hash.
pattern HashChar = 123
-- | Character that represents IVar.
pattern IVarChar = 73
-- | Character that represents Object link.
pattern ObjectLinkChar = 64
-- | Character that represents String.
pattern StringChar = 34
-- | Character that represents Symbol.
pattern SymbolChar = 58
-- | Character that represents Symlink.
pattern SymlinkChar = 59

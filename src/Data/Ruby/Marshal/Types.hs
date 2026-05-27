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
  , pattern BignumChar
  , pattern HashDefChar
  , pattern RegexpChar
  , pattern ObjectChar
  , pattern StructChar
  , pattern ClassChar
  , pattern ModuleChar
  , pattern OldModuleChar
  , pattern ExtendedChar
  , pattern UserDefChar
  , pattern UserMarshalChar
  , pattern UClassChar
  , pattern DataChar
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
-- | Character that represents Bignum.
pattern BignumChar = 108
-- | Character that represents Hash with default value.
pattern HashDefChar = 125
-- | Character that represents Regexp.
pattern RegexpChar = 47
-- | Character that represents Object.
pattern ObjectChar = 111
-- | Character that represents Struct.
pattern StructChar = 83
-- | Character that represents Class reference.
pattern ClassChar = 99
-- | Character that represents Module reference.
pattern ModuleChar = 109
-- | Character that represents the legacy Module/Class reference.
pattern OldModuleChar = 77
-- | Character that represents an object extended with a module.
pattern ExtendedChar = 101
-- | Character that represents a user-defined dump (_dump).
pattern UserDefChar = 117
-- | Character that represents a user-defined marshal (marshal_dump).
pattern UserMarshalChar = 85
-- | Character that represents an object whose class is a user subclass of a builtin.
pattern UClassChar = 67
-- | Character that represents a Data object (_dump_data).
pattern DataChar = 100

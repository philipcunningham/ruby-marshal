{-# LANGUAGE PatternSynonyms #-}

module Data.Ruby.Marshal.Types where

import Data.Vector (Vector)
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
  | RSymbol                !BS.ByteString
    -- ^ represents a @Symbol@
  | RError                 !Error
    -- ^ represents an invalid object
  deriving (Eq, Show)

-- | Allow easy pattern matching of values.
pattern NilC    = 48
pattern TrueC   = 84
pattern FalseC  = 70
pattern FixnumC = 105
pattern ArrayC  = 91
pattern HashC   = 123
pattern IvarC   = 73
pattern StringC = 34
pattern FloatC  = 102
pattern SymbolC = 58

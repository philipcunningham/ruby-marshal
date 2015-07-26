{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Ruby.Marshal.Types where

import Control.Applicative
import Prelude

import Control.Monad.State (lift, MonadState, StateT)
import Data.Serialize.Get  (Get)
import Data.Vector         (Vector)

import qualified Data.ByteString as BS

data Cache = Cache {
    objects :: Vector RubyObject
    -- ^ object cache.
  , symbols :: Vector RubyObject
    -- ^ symbol cache.
  } deriving Show

data Error
  = Unknown
    -- ^ represents an unknown Ruby object
  | Unsupported
    -- ^ represents an unsupported Ruby object
  deriving (Eq, Show)

newtype Marshal a = Marshal {
    runMarshal :: StateT Cache Get a
  } deriving (Functor, Applicative, Monad, MonadState Cache)

liftMarshal :: Get a -> Marshal a
liftMarshal = Marshal . lift

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
  | RIvar                  !(RubyObject, BS.ByteString)
    -- ^ represents an @IVar@
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
pattern ArrayC      = 91
pattern FalseC      = 70
pattern FixnumC     = 105
pattern FloatC      = 102
pattern HashC       = 123
pattern IvarC       = 73
pattern NilC        = 48
pattern ObjectLinkC = 64
pattern StringC     = 34
pattern SymbolC     = 58
pattern SymlinkC    = 59
pattern TrueC       = 84

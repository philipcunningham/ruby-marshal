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
    _objects :: Vector RubyObject
    -- ^ object cache.
  , _symbols :: Vector RubyObject
    -- ^ symbol cache.
  } deriving Show

-- | Convey when unsupported object encountered.
data Error
  = Unsupported
    -- ^ represents an unsupported Ruby object
  deriving (Eq, Show)

-- | Marshal monad endows the underling Get monad with State.
newtype Marshal a = Marshal {
  runMarshal :: StateT Cache Get a
  } deriving (Functor, Applicative, Monad, MonadState Cache)

-- | Lift Get monad into Marshal monad.
liftMarshal :: Get a -> Marshal a
liftMarshal = Marshal . lift

-- | Representation of a Ruby object.
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
  | RIVar                  !(RubyObject, BS.ByteString)
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

-- http://docs.ruby-lang.org/en/2.1.0/marshal_rdoc.html#label-Stream+Format

-- | NilClass
pattern NilC = 48
-- | FalseClass
pattern FalseC = 70
-- | TrueClass
pattern TrueC = 84
-- | Array
pattern ArrayC = 91
-- | Fixnum
pattern FixnumC = 105
-- | Float
pattern FloatC = 102
-- | Hash
pattern HashC = 123
-- | IVar
pattern IVarC = 73
-- | Object link
pattern ObjectLinkC = 64
-- | String
pattern StringC = 34
-- | Symbol
pattern SymbolC = 58
-- | Symlink
pattern SymlinkC = 59

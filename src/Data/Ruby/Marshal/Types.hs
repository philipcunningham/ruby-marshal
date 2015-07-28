{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Ruby.Marshal.Types where

import Control.Applicative
import Prelude

import Control.Monad.State (lift, MonadState, StateT)
import Data.Map            (Map)
import Data.Serialize.Get  (Get)
import Data.Vector         (Vector)

import qualified Data.ByteString as BS

-- | Character that represents NilClass.
pattern NilC = 48
-- | Character that represents FalseClass.
pattern FalseC = 70
-- | Character that represents TrueClass.
pattern TrueC = 84
-- | Character that represents Array.
pattern ArrayC = 91
-- | Character that represents Fixnum.
pattern FixnumC = 105
-- | Character that represents Float.
pattern FloatC = 102
-- | Character that represents Hash.
pattern HashC = 123
-- | Character that represents IVar.
pattern IVarC = 73
-- | Character that represents Object link.
pattern ObjectLinkC = 64
-- | Character that represents String.
pattern StringC = 34
-- | Character that represents Symbol.
pattern SymbolC = 58
-- | Character that represents Symlink.
pattern SymlinkC = 59

-- | State that we must carry around during parsing.
data Cache = Cache {
    _objects :: !(Vector RubyObject)
    -- ^ object cache.
  , _symbols :: !(Vector RubyObject)
    -- ^ symbol cache.
  } deriving Show

-- | Marshal monad endows the underlying Get monad with State.
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
  | RHash                  !(Map RubyObject RubyObject)
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
  deriving (Eq, Ord, Show)

-- | Convey when unsupported object encountered.
data Error
  = Unsupported
    -- ^ represents an unsupported Ruby object
  deriving (Eq, Ord, Show)

class Ruby a where
  toRuby   :: a -> RubyObject
  fromRuby :: RubyObject -> Maybe a

instance Ruby RubyObject where
  toRuby = id
  fromRuby = Just

instance Ruby () where
  toRuby _ = RNil
  fromRuby = \case
    RNil -> Just ()
    _    -> Nothing

instance Ruby Bool where
  toRuby = RBool
  fromRuby = \case
    RBool x -> Just x
    _       -> Nothing

instance Ruby Int where
  toRuby = RFixnum
  fromRuby = \case
    RFixnum x -> Just x
    _         -> Nothing

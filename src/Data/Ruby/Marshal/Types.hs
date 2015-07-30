{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}
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

module Data.Ruby.Marshal.Types where

import Control.Applicative
import Prelude

import Control.Arrow       ((***))
import Control.Monad.State (lift, MonadState, StateT)
import Data.Serialize.Get  (Get)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as DM
import qualified Data.Vector     as V

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
    _objects :: !(V.Vector RubyObject)
    -- ^ object cache.
  , _symbols :: !(V.Vector RubyObject)
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
  | RArray                 !(V.Vector RubyObject)
    -- ^ represents an @Array@
  | RHash                  !(V.Vector (RubyObject, RubyObject))
    -- ^ represents an @Hash@
  | RIVar                  !(RubyObject, BS.ByteString)
    -- ^ represents an @IVar@
  | RString                !BS.ByteString
    -- ^ represents a @String@
  | RFloat {-# UNPACK #-}  !Float
    -- ^ represents a @Float@
  | RSymbol                !BS.ByteString
    -- ^ represents a @Symbol@
  | RError                 !Error
    -- ^ represents an invalid object
  deriving (Eq, Show)

-- | Convey when unsupported object encountered.
data Error
  = Unsupported
    -- ^ represents an unsupported Ruby object
  deriving (Eq, Ord, Show)

-- | Transform plain Haskell values to RubyObjects and back.
class Rubyable a where
  -- | Takes a plain Haskell value and lifts into RubyObject
  toRuby :: a -> RubyObject
  -- | Takes a RubyObject transforms it into a more general Haskell value.
  fromRuby :: RubyObject -> Maybe a

-- core instances

instance Rubyable RubyObject where
  toRuby = id
  fromRuby = Just

instance Rubyable () where
  toRuby _ = RNil
  fromRuby = \case
    RNil -> Just ()
    _    -> Nothing

instance Rubyable Bool where
  toRuby = RBool
  fromRuby = \case
    RBool x -> Just x
    _       -> Nothing

instance Rubyable Int where
  toRuby = RFixnum
  fromRuby = \case
    RFixnum x -> Just x
    _         -> Nothing

instance Rubyable a => Rubyable (V.Vector a) where
  toRuby = RArray . V.map toRuby
  fromRuby = \case
    RArray x -> V.mapM fromRuby x
    _        -> Nothing

instance (Rubyable a, Rubyable b) => Rubyable (V.Vector (a, b)) where
  toRuby x = RHash $ V.map (toRuby *** toRuby) x
  fromRuby = \case
    RHash x -> V.mapM (\(k, v) -> (,) <$> fromRuby k <*> fromRuby v) x
    _       -> Nothing

instance Rubyable BS.ByteString where
  toRuby = RSymbol
  fromRuby = \case
    RSymbol x -> Just x
    _         -> Nothing

instance Rubyable Float where
  toRuby = RFloat
  fromRuby = \case
    RFloat  x -> Just x
    _         -> Nothing

-- map like

instance (Rubyable a, Rubyable b) => Rubyable [(a, b)] where
  toRuby = toRuby . V.fromList
  fromRuby x = V.toList <$> fromRuby x

instance (Rubyable a, Rubyable b, Ord a) => Rubyable (DM.Map a b) where
  toRuby = toRuby . DM.toList
  fromRuby x = DM.fromList <$> fromRuby x

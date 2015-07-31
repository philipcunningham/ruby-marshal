{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
    -- * Re-exported modules
    module Data.Ruby.Marshal.RubyObject
  , module Data.Ruby.Marshal.Types
) where

import Control.Applicative
import Data.Ruby.Marshal.RubyObject
import Prelude

import Control.Monad.State (lift, MonadState, StateT)
import Data.Serialize.Get  (Get)

import qualified Data.Vector as V

-- | Marshal monad endows the underlying Get monad with State.
newtype Marshal a = Marshal {
  runMarshal :: StateT Cache Get a
} deriving (Functor, Applicative, Monad, MonadState Cache)

-- | Lift Get monad into Marshal monad.
liftMarshal :: Get a -> Marshal a
liftMarshal = Marshal . lift

-- | State that we must carry around during parsing.
data Cache = Cache {
    _objects :: !(V.Vector RubyObject)
    -- ^ object cache.
  , _symbols :: !(V.Vector RubyObject)
    -- ^ symbol cache.
} deriving Show

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

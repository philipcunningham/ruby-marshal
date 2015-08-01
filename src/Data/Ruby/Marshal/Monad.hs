{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Ruby.Marshal.Monad where

import Control.Monad.State          (lift, MonadState, StateT)
import Data.Ruby.Marshal.RubyObject (RubyObject)
import Data.Serialize.Get           (Get)

import qualified Data.Vector as V

-- | State that we must carry around during parsing.
data Cache = Cache {
    _objects :: !(V.Vector RubyObject)
    -- ^ object cache.
  , _symbols :: !(V.Vector RubyObject)
    -- ^ symbol cache.
} deriving Show

-- | Constructs an empty cache to store symbols and objects.
emptyCache :: Cache
emptyCache = Cache { _symbols = V.empty, _objects = V.empty }

-- | Marshal monad endows the underlying Get monad with State.
newtype Marshal a = Marshal {
  runMarshal :: StateT Cache Get a
} deriving (Functor, Applicative, Monad, MonadState Cache)

-- | Lift Get monad into Marshal monad.
liftMarshal :: Get a -> Marshal a
liftMarshal = Marshal . lift

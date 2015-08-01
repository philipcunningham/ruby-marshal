{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.Ruby.Marshal.Monad where

import Control.Monad.State          (get, gets, lift, put, MonadState, StateT)
import Data.Ruby.Marshal.RubyObject (RubyObject(..))
import Data.Serialize.Get           (Get)

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
    objects :: !(V.Vector RubyObject)
    -- ^ object cache.
  , symbols :: !(V.Vector RubyObject)
    -- ^ symbol cache.
} deriving Show

-- | Constructs an empty cache to store symbols and objects.
emptyCache :: Cache
emptyCache = Cache { symbols = V.empty, objects = V.empty }

-- | Look up object in our object cache.
readObject :: Int -> Marshal (Maybe RubyObject)
readObject index = gets objects >>= \objectCache ->
  return $ objectCache V.!? index

-- | Look up a symbol in our symbol cache.
readSymbol :: Int -> Marshal (Maybe RubyObject)
readSymbol index = gets symbols >>= \symbolCache ->
  return $ symbolCache V.!? index

-- | Write an object to the appropriate cache.
writeCache :: RubyObject -> Marshal ()
writeCache object = do
  cache <- get
  case object of
    RIVar   _ -> put $ cache { objects = V.snoc (objects cache) object }
    RSymbol _ -> put $ cache { symbols = V.snoc (symbols cache) object }
    _         -> return ()

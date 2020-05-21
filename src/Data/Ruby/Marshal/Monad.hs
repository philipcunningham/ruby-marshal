{-# LANGUAGE CPP                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.Ruby.Marshal.Monad
-- Copyright : (c) Philip Cunningham, 2015
-- License   : MIT
--
-- Maintainer:  802389+philipcunningham@users.noreply.github.com
-- Stability :  experimental
-- Portability: portable
--
-- Marshal monad provides an object cache over the Get monad.
--
--------------------------------------------------------------------

module Data.Ruby.Marshal.Monad where

import           Control.Applicative
import qualified Control.Monad.Fail           as Fail
import qualified Control.Monad                as Monad
import           Control.Monad                (join)
import           Control.Monad.State.Strict   (MonadState, StateT, get, gets,
                                               lift, put)
import           Data.Ruby.Marshal.RubyObject (RubyObject (..))
import           Data.Serialize.Get           (Get)
import           Data.Vector                  (Vector)
import qualified Data.Vector                  as V
import           Prelude

-- | Marshal monad endows the underlying Get monad with State.
newtype Marshal a = Marshal {
  runMarshal :: StateT Cache Get a
} deriving (Functor, Applicative, MonadState Cache)

instance Monad Marshal where
  (Marshal ma) >>= f = Marshal . join $ runMarshal . f <$> ma

#if !MIN_VERSION_base(4,13,0)
  fail = Fail.fail
#endif

instance Fail.MonadFail Marshal where
  fail = Marshal . Monad.fail

-- | Lift Get monad into Marshal monad.
liftMarshal :: Get a -> Marshal a
liftMarshal = Marshal . lift

-- | State that we must carry around during deserialisation.
data Cache = Cache {
    objects :: !(Vector RubyObject)
    -- ^ object cache.
  , symbols :: !(Vector RubyObject)
    -- ^ symbol cache.
} deriving Show

-- | Constructs an empty cache to store symbols and objects.
emptyCache :: Cache
emptyCache = Cache { symbols = V.empty, objects = V.empty }

-- | Look up value in cache.
readCache :: Int -> (Cache -> Vector RubyObject) -> Marshal (Maybe RubyObject)
readCache index f = gets f >>= \cache -> return $ cache V.!? index

-- | Look up object in object cache.
readObject :: Int -> Marshal (Maybe RubyObject)
readObject index = readCache index objects

-- | Look up a symbol in symbol cache.
readSymbol :: Int -> Marshal (Maybe RubyObject)
readSymbol index = readCache index symbols

-- | Write an object to the appropriate cache.
writeCache :: RubyObject -> Marshal ()
writeCache object = do
  cache <- get
  case object of
    RIVar _   -> put $ cache { objects = V.snoc (objects cache) object }
    RSymbol _ -> put $ cache { symbols = V.snoc (symbols cache) object }
    _         -> return ()

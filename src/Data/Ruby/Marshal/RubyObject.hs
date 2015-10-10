{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE LambdaCase #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.Ruby.Marshal.RubyObject
-- Copyright : (c) Philip Cunningham, 2015
-- License   : MIT
--
-- Maintainer:  802389+philipcunningham@users.noreply.github.com
-- Stability :  experimental
-- Portability: portable
--
-- Core RubyObject data representation.
--
--------------------------------------------------------------------

module Data.Ruby.Marshal.RubyObject where

import           Control.Applicative
import           Control.Arrow ((***))
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as DM
import           Data.Ruby.Marshal.Encoding (RubyStringEncoding(..))
import qualified Data.Vector as V
import           Prelude

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
  | RIVar                  !(RubyObject, RubyStringEncoding)
    -- ^ represents an @IVar@
  | RString                !BS.ByteString
    -- ^ represents a @String@
  | RFloat {-# UNPACK #-}  !Float
    -- ^ represents a @Float@
  | RSymbol                !BS.ByteString
    -- ^ represents a @Symbol@
  | Unsupported
    -- ^ represents an invalid object
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
    RFloat x -> Just x
    _        -> Nothing

instance Rubyable (BS.ByteString, RubyStringEncoding) where
  toRuby (x, y) = RIVar (RString x, y)
  fromRuby = \case
    RIVar (RString x, y) -> Just (x, y)
    _                    -> Nothing

-- nil like

instance Rubyable a => Rubyable (Maybe a) where
  toRuby = \case
    Just x  -> toRuby x
    Nothing -> RNil
  fromRuby = \case
    RNil -> Just Nothing
    x    -> fromRuby x

-- array like

instance Rubyable a => Rubyable [a] where
  toRuby = toRuby . V.fromList
  fromRuby x = V.toList <$> fromRuby x

-- map like

instance (Rubyable a, Rubyable b) => Rubyable [(a, b)] where
  toRuby = toRuby . V.fromList
  fromRuby x = V.toList <$> fromRuby x

instance (Rubyable a, Rubyable b, Ord a) => Rubyable (DM.Map a b) where
  toRuby = toRuby . DM.toList
  fromRuby x = DM.fromList <$> fromRuby x

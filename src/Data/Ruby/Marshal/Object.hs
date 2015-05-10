{-# LANGUAGE MultiWayIf #-}

module Data.Ruby.Marshal.Object (
  RubyObject(..),
  getRubyObject
) where

import Control.Applicative
import Data.Ruby.Marshal.Get

import Data.Serialize.Get  (Get, getWord8, lookAhead)
import Data.Vector         (Vector)
import Prelude

data Error = Unknown | Unsupported deriving (Eq, Show)

data RubyObject = RNil
                  | RBool                       !Bool
                  | RFixnum      {-# UNPACK #-} !Int
                  | RArray                      !(Vector RubyObject)
                  | RHash                       !(Vector (RubyObject, RubyObject))
                  | RError                      !Error
                  deriving (Eq, Show)

getRubyObject :: Get RubyObject
getRubyObject = do
  c <- lookAhead getWord8
  if | c == 48            -> RNil    <$  getNil
     | c == 70 || c == 84 -> RBool   <$> getBool
     | c == 105           -> RFixnum <$> getFixnum
     | c == 91            -> RArray  <$> getArray getRubyObject
     | c == 123           -> RHash   <$> getHash getRubyObject getRubyObject
     | otherwise          -> return   $  RError Unsupported

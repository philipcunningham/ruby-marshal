{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Ruby.Marshal.Object (
  RubyObject(..),
  getRubyObject
) where

import Control.Applicative
import Data.Ruby.Marshal.Get
import Prelude

import Data.Serialize.Get  (Get, getWord8, lookAhead)
import Data.Vector         (Vector)

import qualified Data.ByteString as BS

data Error = Unknown | Unsupported deriving (Eq, Show)

data RubyObject = RNil
                  | RBool                       !Bool
                  | RFixnum      {-# UNPACK #-} !Int
                  | RArray                      !(Vector RubyObject)
                  | RHash                       !(Vector (RubyObject, RubyObject))
                  | RString                     !BS.ByteString
                  | RError                      !Error
                  deriving (Eq, Show)

pattern MNil       = 48
pattern MTrue      = 70
pattern MFalse     = 84
pattern MFixnum    = 105
pattern MArray     = 91
pattern MHash      = 123
pattern MIvar      = 73
pattern MRawString = 34

getRubyObject :: Get RubyObject
getRubyObject = do
  c <- lookAhead getWord8
  case c of
   MNil    -> RNil    <$  getNil
   MTrue   -> RBool   <$> getBool
   MFalse  -> RBool   <$> getBool
   MFixnum -> RFixnum <$> getFixnum
   MArray  -> RArray  <$> getArray getRubyObject
   MHash   -> RHash   <$> getHash getRubyObject getRubyObject
   MIvar   -> do
     ivarc <- lookAhead (getWord8 >> getWord8)
     case ivarc of
       MRawString -> RString <$> getString
       _          -> return   $  RError Unsupported
   _       -> return   $  RError Unsupported

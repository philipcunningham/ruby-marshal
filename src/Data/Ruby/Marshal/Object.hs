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

import Data.Serialize.Get  (Get, getWord8)
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
pattern MTrue      = 84
pattern MFalse     = 70
pattern MFixnum    = 105
pattern MArray     = 91
pattern MHash      = 123
pattern MIvar      = 73
pattern MRawString = 34

getRubyObject :: Get RubyObject
getRubyObject = do
  c <- getWord8
  case c of
   MNil    -> return RNil
   MTrue   -> return $ RBool True
   MFalse  -> return $ RBool False
   MFixnum -> RFixnum <$> getFixnum
   MArray  -> RArray  <$> getArray getRubyObject
   MHash   -> RHash   <$> getHash getRubyObject getRubyObject
   MIvar   -> do
     ivarc <- getWord8
     case ivarc of
       MRawString -> RString <$> getString
       _          -> getRubyObject
   _       -> return $ RError Unsupported

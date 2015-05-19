{-# LANGUAGE LambdaCase #-}
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
                  | RFloat                      !Double
                  | RError                      !Error
                  deriving (Eq, Show)

pattern CNil    = 48
pattern CTrue   = 84
pattern CFalse  = 70
pattern CFixnum = 105
pattern CArray  = 91
pattern CHash   = 123
pattern CIvar   = 73
pattern CString = 34
pattern CFloat  = 102

getRubyObject :: Get RubyObject
getRubyObject = getWord8 >>= \case
  CNil    -> return RNil
  CTrue   -> return $ RBool True
  CFalse  -> return $ RBool False
  CFixnum -> RFixnum <$> getFixnum
  CArray  -> RArray  <$> getArray getRubyObject
  CHash   -> RHash   <$> getHash getRubyObject getRubyObject
  CIvar   -> getWord8 >>= \case CString -> RString <$> getString getRubyObject
                                _       -> unsupported
  CFloat  -> RFloat <$> getFloat
  _       -> unsupported
  where
    unsupported = return $ RError Unsupported

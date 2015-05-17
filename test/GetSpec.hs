{-# LANGUAGE OverloadedStrings #-}

module GetSpec (spec) where

import Data.Ruby.Marshal.Get
import Test.Hspec

import Data.Serialize.Get (runGet)

import qualified Data.ByteString as BS

nil :: BS.ByteString
nil = BS.pack [48]

spec :: Spec
spec = describe "getNil" $
  context "when we have nil" $
    it "should parse" $
      runGet getNil nil `shouldBe` Right ()

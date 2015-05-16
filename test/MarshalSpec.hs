{-# LANGUAGE OverloadedStrings #-}

module MarshalSpec (spec) where

import Data.Ruby.Marshal
import Test.Hspec

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

loadBin :: FilePath -> IO (Either String RubyObject)
loadBin path = do
    bs <- BS.readFile path
    return $ load bs

spec :: Spec
spec = describe "load" $ do
  context "when we have nil" $ do
    it "should parse" $ do
      object <- loadBin "test/bin/nil.bin"
      object `shouldBe` Right (RNil)

  context "when we have true" $ do
    it "should parse" $ do
      object <- loadBin "test/bin/true.bin"
      object `shouldBe` Right (RBool True)

  context "when we have false" $ do
    it "should parse" $ do
      object <- loadBin "test/bin/false.bin"
      object `shouldBe` Right (RBool False)

  context "when we have 0" $ do
    it "should parse" $ do
      object <- loadBin "test/bin/0.bin"
      object `shouldBe` Right (RFixnum 0)

  context "when we have -42" $ do
    it "should parse" $ do
      object <- loadBin "test/bin/neg42.bin"
      object `shouldBe` Right (RFixnum (-42))

  context "when we have 42" $ do
    it "should parse" $ do
      object <- loadBin "test/bin/42.bin"
      object `shouldBe` Right (RFixnum 42)

  context "when we have -2048" $ do
    it "should parse" $ do
      object <- loadBin "test/bin/neg2048.bin"
      object `shouldBe` Right (RFixnum (-2048))

  context "when we have 2048" $ do
    it "should parse" $ do
      object <- loadBin "test/bin/2048.bin"
      object `shouldBe` Right (RFixnum 2048)

  context "when we have [nil]" $ do
    it "should parse" $ do
      object <- loadBin "test/bin/nilArray.bin"
      object `shouldBe` Right (RArray $ V.fromList [RNil])

  context "when we have [true, false]" $ do
    it "should parse" $ do
      object <- loadBin "test/bin/boolArray.bin"
      object `shouldBe` Right (RArray $ V.fromList [RBool True, RBool False])

  context "when we have [-2048, -42, 0, 42, 2048]" $ do
    it "should parse" $ do
      object <- loadBin "test/bin/fixnumArray.bin"
      object `shouldBe` Right (RArray $ V.fromList [RFixnum (-2048), RFixnum (-42), RFixnum 0, RFixnum 42, RFixnum 2048])

  context "when we have { 0 => false, 1 => true }" $ do
    it "should parse" $ do
      object <- loadBin "test/bin/fixnumHash.bin"
      object `shouldBe` Right (RHash $ V.fromList [(RFixnum 0, RBool False), (RFixnum 1, RBool True)])

  context "when we have 'hello haskell'" $ do
    it "should parse" $ do
      object <- loadBin "test/bin/rawString.bin"
      object `shouldBe` Right (RString "hello haskell")

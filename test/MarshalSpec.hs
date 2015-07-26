{-# LANGUAGE OverloadedStrings #-}

module MarshalSpec (spec) where

import Data.Ruby.Marshal
import Test.Hspec

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

loadBin :: FilePath -> IO (Maybe RubyObject)
loadBin path = do
    bs <- BS.readFile path
    return $ load bs

spec :: Spec
spec = describe "load" $ do
  context "when we have nil" $
    it "should parse" $ do
      object <- loadBin "test/bin/nil.bin"
      object `shouldBe` Just RNil

  context "when we have true" $
    it "should parse" $ do
      object <- loadBin "test/bin/true.bin"
      object `shouldBe` Just (RBool True)

  context "when we have false" $
    it "should parse" $ do
      object <- loadBin "test/bin/false.bin"
      object `shouldBe` Just (RBool False)

  context "when we have 0" $
    it "should parse" $ do
      object <- loadBin "test/bin/0.bin"
      object `shouldBe` Just (RFixnum 0)

  context "when we have -42" $
    it "should parse" $ do
      object <- loadBin "test/bin/neg42.bin"
      object `shouldBe` Just (RFixnum (-42))

  context "when we have 42" $
    it "should parse" $ do
      object <- loadBin "test/bin/42.bin"
      object `shouldBe` Just (RFixnum 42)

  context "when we have -2048" $
    it "should parse" $ do
      object <- loadBin "test/bin/neg2048.bin"
      object `shouldBe` Just (RFixnum (-2048))

  context "when we have 2048" $
    it "should parse" $ do
      object <- loadBin "test/bin/2048.bin"
      object `shouldBe` Just (RFixnum 2048)

  context "when we have [nil]" $
    it "should parse" $ do
      object <- loadBin "test/bin/nilArray.bin"
      object `shouldBe` Just (RArray $ V.fromList [RNil])

  context "when we have [true, false]" $
    it "should parse" $ do
      object <- loadBin "test/bin/boolArray.bin"
      object `shouldBe` Just (RArray $ V.fromList [RBool True, RBool False])

  context "when we have [-2048, -42, 0, 42, 2048]" $
    it "should parse" $ do
      object <- loadBin "test/bin/fixnumArray.bin"
      object `shouldBe` Just (RArray $ V.fromList [RFixnum (-2048), RFixnum (-42), RFixnum 0, RFixnum 42, RFixnum 2048])

  context "when we have ['hello', 'haskell', 'hello', 'haskell']" $
    it "should parse" $ do
      object <- loadBin "test/bin/stringArray.bin"
      object `shouldBe` Just (RArray $ V.fromList [RIvar (RString "hello", "UTF-8"), RIvar (RString "haskell", "UTF-8"), RIvar (RString "hello", "UTF-8"), RIvar (RString "haskell", "UTF-8")])

  context "when we have [:hello, :haskell, :hello, :haskell]" $
    it "should parse" $ do
      object <- loadBin "test/bin/symbolArray.bin"
      object `shouldBe` Just (RArray $ V.fromList [RSymbol "hello", RSymbol "haskell", RSymbol "hello", RSymbol "haskell"])

  context "when we have { 0 => false, 1 => true }" $
    it "should parse" $ do
      object <- loadBin "test/bin/fixnumHash.bin"
      object `shouldBe` Just (RHash $ V.fromList [(RFixnum 0, RBool False), (RFixnum 1, RBool True)])

  context "when we have 'hello haskell'" $
    it "should parse" $ do
      object <- loadBin "test/bin/UTF_8_String.bin"
      object `shouldBe` Just (RIvar (RString "hello haskell", "UTF-8"))

  context "when we have 'hello haskell' in US-ASCII" $
    it "should parse" $ do
      object <- loadBin "test/bin/US_ASCII_String.bin"
      object `shouldBe` Just (RIvar (RString "hello haskell", "US-ASCII"))

  context "when we have 'hello haskell' in SHIFT_JIS" $
    it "should parse" $ do
      object <- loadBin "test/bin/Shift_JIS_String.bin"
      object `shouldBe` Just (RIvar (RString "hello haskell", "Shift_JIS"))

  context "when we have 3.33333" $
    it "should parse" $ do
      object <- loadBin "test/bin/float.bin"
      object `shouldBe` Just (RFloat 3.33333)

  context "when we have :hello_haskell" $
    it "should parse" $ do
      object <- loadBin "test/bin/symbol.bin"
      object `shouldBe` Just (RSymbol "hello_haskell")

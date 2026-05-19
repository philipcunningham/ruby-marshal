{-# LANGUAGE OverloadedStrings #-}

module MarshalSpec (spec) where

import Data.Ruby.Marshal
import Test.Hspec

import qualified Data.ByteString as BS
import qualified Data.Vector     as V

loadBin :: FilePath -> IO (Maybe RubyObject)
loadBin path = do
    bs <- BS.readFile path
    return $ decode bs

loadBinEither :: FilePath -> IO (Either String RubyObject)
loadBinEither path = do
    bs <- BS.readFile path
    return $ decodeEither bs

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
      object `shouldBe` Just (RArray $ V.fromList [RIVar (RString "hello", UTF_8), RIVar (RString "haskell", UTF_8), RIVar (RString "hello", UTF_8), RIVar (RString "haskell", UTF_8)])

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
      object `shouldBe` Just (RIVar (RString "hello haskell", UTF_8))

  context "when we have 'hello haskell' in US-ASCII" $
    it "should parse" $ do
      object <- loadBin "test/bin/US_ASCII_String.bin"
      object `shouldBe` Just (RIVar (RString "hello haskell", US_ASCII))

  context "when we have 'hello haskell' in SHIFT_JIS" $
    it "should parse" $ do
      object <- loadBin "test/bin/Shift_JIS_String.bin"
      object `shouldBe` Just (RIVar (RString "hello haskell", Shift_JIS))

  context "when we have 3.33333" $
    it "should parse" $ do
      object <- loadBin "test/bin/float.bin"
      object `shouldBe` Just (RFloat 3.33333)

  context "when we have :hello_haskell" $
    it "should parse" $ do
      object <- loadBin "test/bin/symbol.bin"
      object `shouldBe` Just (RSymbol "hello_haskell")

  context "when we have hashes, arrays and object links" $
    it "should parse" $ do
      object <- loadBinEither "test/bin/objectsAndStringReferences.bin"
      object `shouldBe` Right (RArray $ V.fromList
        [ RHash mempty, RArray mempty, RIVar (RString "hello", UTF_8), RIVar (RString "haskell", UTF_8)
        , RHash mempty, RArray mempty, RIVar (RString "hello", UTF_8), RIVar (RString "haskell", UTF_8)])

  context "when we have a positive Bignum (2**40)" $
    it "should parse" $ do
      object <- loadBin "test/bin/bignum.bin"
      object `shouldBe` Just (RBignum 1099511627776)

  context "when we have a negative Bignum (-(2**40))" $
    it "should parse" $ do
      object <- loadBin "test/bin/negativeBignum.bin"
      object `shouldBe` Just (RBignum (-1099511627776))

  context "when we have a Regexp /fo+/i" $
    it "should parse the inner Regexp, discarding the IVar encoding wrapper" $ do
      object <- loadBin "test/bin/regexp.bin"
      -- options=1 (IGNORECASE); the enclosing IVar's encoding info is dropped
      -- because the inner is not a String.
      object `shouldBe` Just (RRegexp "fo+" 1)

  context "when we have a Hash with a default value" $
    it "should parse" $ do
      object <- loadBin "test/bin/hashWithDefault.bin"
      object `shouldBe` Just
        (RHashWithDefault
          (V.fromList [(RFixnum 1, RFixnum 10), (RFixnum 2, RFixnum 20)])
          (RFixnum 0))

  context "when we have a Class reference" $
    it "should parse" $ do
      object <- loadBin "test/bin/classRef.bin"
      object `shouldBe` Just (RClass "Array")

  context "when we have a Module reference" $
    it "should parse" $ do
      object <- loadBin "test/bin/moduleRef.bin"
      object `shouldBe` Just (RModule "Comparable")

  context "when we have a plain Object (Point.new(1, 2))" $
    it "should parse the class name and instance variables" $ do
      object <- loadBin "test/bin/object.bin"
      object `shouldBe` Just
        (RObject "Point"
          (V.fromList [(RSymbol "@x", RFixnum 1), (RSymbol "@y", RFixnum 2)]))

  context "when we have a Struct (PointStruct.new(3, 4))" $
    it "should parse" $ do
      object <- loadBin "test/bin/struct.bin"
      object `shouldBe` Just
        (RStruct "PointStruct"
          (V.fromList [(RSymbol "x", RFixnum 3), (RSymbol "y", RFixnum 4)]))

  context "when we have a UserDef object (Packed via _dump)" $
    it "should expose the class name and opaque payload bytes" $ do
      object <- loadBin "test/bin/userDef.bin"
      -- [42].pack("L") = "*\\0\\0\\0" on a little-endian host.
      object `shouldBe` Just (RUserDef "Packed" "\x2a\x00\x00\x00")

  context "when we have a UserMarshal object (Boxed via marshal_dump)" $
    it "should expose the class name and dumped payload" $ do
      object <- loadBin "test/bin/userMarshal.bin"
      object `shouldBe` Just (RUserMarshal "Boxed" (RFixnum 42))

  context "when we have a String with an extra non-encoding instance variable" $
    it "should still surface the String+encoding and not corrupt the stream" $ do
      -- Previously the IVar parser failed when len /= 1; now it consumes all
      -- IV bytes and just picks the encoding it understands.
      object <- loadBin "test/bin/stringWithExtraIVar.bin"
      object `shouldBe` Just (RIVar (RString "hello", UTF_8))

  context "when we have a String extended with a module (tag 'e')" $
    it "should pass the inner String through, dropping the module info" $ do
      object <- loadBin "test/bin/extendedString.bin"
      object `shouldBe` Just (RIVar (RString "extended", UTF_8))

  context "when we have a subclassed Array (tag 'C')" $
    it "should pass the inner Array through, dropping the subclass info" $ do
      object <- loadBin "test/bin/subclassedArray.bin"
      object `shouldBe` Just (RArray (V.fromList [RFixnum 1, RFixnum 2]))

  context "when an object-link points back at a previously-parsed Object" $
    it "should resolve the link to the cached Object" $ do
      -- Exercises that the new RObject constructor participates in the
      -- object cache so that @N references still work in mixed-type arrays.
      let point = RObject "Point"
                    (V.fromList [(RSymbol "@x", RFixnum 1), (RSymbol "@y", RFixnum 2)])
      object <- loadBin "test/bin/objectLinkArray.bin"
      object `shouldBe` Just
        (RArray (V.fromList [point, RIVar (RString "marker", UTF_8), point]))

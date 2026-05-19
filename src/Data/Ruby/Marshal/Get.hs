{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

--------------------------------------------------------------------
-- |
-- Module    : Data.Ruby.Marshal.Get
-- Copyright : (c) Philip Cunningham, 2015
-- License   : MIT
--
-- Maintainer:  802389+philipcunningham@users.noreply.github.com
-- Stability :  experimental
-- Portability: portable
--
-- Parsers for Ruby Marshal format.
--
--------------------------------------------------------------------

module Data.Ruby.Marshal.Get (
    -- * Ruby Marshal parsers
    getMarshalVersion
  , getRubyObject
) where

import           Control.Applicative
import           Control.Monad              (liftM2, when)
import           Data.Monoid                ((<>))
import qualified Data.ByteString            as BS
import           Data.Ruby.Marshal.Encoding (toEnc)
import           Data.Ruby.Marshal.Int
import           Data.Ruby.Marshal.Monad    (liftMarshal, readObject,
                                             readSymbol, writeCache)
import           Data.Ruby.Marshal.Types
import           Data.Serialize.Get         (Get, getBytes, getTwoOf, label)
import           Data.String.Conv           (toS)
import qualified Data.Vector                as V
import           Prelude
import           Text.Read                  (readMaybe)

--------------------------------------------------------------------
-- Top-level functions.

-- | Parses Marshal version.
getMarshalVersion :: Marshal (Word8, Word8)
getMarshalVersion = liftAndLabel "Marshal Version" $
  getTwoOf getWord8 getWord8 >>= \version -> case version of
    (4, 8) -> return version
    _      -> fail "marshal version unsupported"

-- | Parses a subset of Ruby objects.
getRubyObject :: Marshal RubyObject
getRubyObject = getMarshalVersion >> go
  where
    go :: Marshal RubyObject
    go = liftMarshal getWord8 >>= \case
           NilChar         -> return RNil
           TrueChar        -> return $ RBool True
           FalseChar       -> return $ RBool False
           FixnumChar      -> RFixnum <$> getFixnum
           FloatChar       -> RFloat <$> getFloat
           StringChar      -> RString <$> getString
           SymbolChar      -> RSymbol <$> getSymbol
           ObjectLinkChar  -> getObjectLink
           SymlinkChar     -> RSymbol <$> getSymlink
           ArrayChar       -> do
             result <- RArray <$> getArray go
             writeCache result
             pure result
           HashChar        -> do
             result <- RHash <$> getHash go go
             writeCache result
             pure result
           HashDefChar     -> getHashWithDefault go
           IVarChar        -> getIVar go
           BignumChar      -> getBignum
           RegexpChar      -> getRegexp
           ObjectChar      -> getObjectOrStruct RObject "Object" go
           StructChar      -> getObjectOrStruct RStruct "Struct" go
           ClassChar       -> getNamedRef RClass "Class"
           ModuleChar      -> getNamedRef RModule "Module"
           OldModuleChar   -> getNamedRef RModule "OldModule"
           UserDefChar     -> getUserDef go
           UserMarshalChar -> getUserMarshalOrData RUserMarshal "UserMarshal" go
           DataChar        -> getUserMarshalOrData RData "Data" go
           ExtendedChar    -> getWrapper "Extended" go
           UClassChar      -> getWrapper "UClass" go
           c               -> fail $ "unknown marshal tag: " <> show c

--------------------------------------------------------------------
-- Ancillary functions.

-- | Parses <http://ruby-doc.org/core-2.2.0/Array.html Array>.
getArray :: Marshal a -> Marshal (V.Vector a)
getArray g = marshalLabel "Fixnum" $ do
  n <- getFixnum
  V.replicateM n g

-- | Parses <http://ruby-doc.org/core-2.2.0/Fixnum.html Fixnum>.
getFixnum :: Marshal Int
getFixnum = liftAndLabel "Fixnum" $ do
  x <- getInt8
  if | x == 0 -> fromIntegral <$> return x
     | x == 1 -> fromIntegral <$> getWord8
     | x == -1 -> fromIntegral <$> getNegInt16
     | x == 2 -> fromIntegral <$> getWord16le
     | x == -2 -> fromIntegral <$> getInt16le
     | x == 3 -> fromIntegral <$> getWord24le
     | x == -3 -> fromIntegral <$> getInt24le
     | x == 4 -> fromIntegral <$> getWord32le
     | x == -4 -> fromIntegral <$> getInt32le
     | x >= 6 -> fromIntegral <$> return (x - 5)
     | x <= -6 -> fromIntegral <$> return (x + 5)
     | otherwise -> empty
  where
    getNegInt16 :: Get Int16
    getNegInt16 = do
      x <- fromIntegral <$> getInt8
      if x >= 0 && x <= 127
        then return (x - 256)
        else return x

-- | Parses <http://ruby-doc.org/core-2.2.0/Float.html Float>.
getFloat :: Marshal Float
getFloat = marshalLabel "Float" $ do
  s <- getString
  case readMaybe . toS $ s of
    Just float -> return float
    Nothing    -> fail "expected float"

-- | Parses <http://ruby-doc.org/core-2.2.0/Hash.html Hash>.
getHash :: Marshal a -> Marshal b -> Marshal (V.Vector (a, b))
getHash k v = marshalLabel "Hash" $ do
  n <- getFixnum
  V.replicateM n (liftM2 (,) k v)

-- | Parses <http://docs.ruby-lang.org/en/2.1.0/marshal_rdoc.html#label-Instance+Variables Instance Variables>.
--
-- IVar wraps an arbitrary object together with a list of @(symbol, value)@
-- instance-variable pairs. When the wrapped object is a string and the IVs
-- carry encoding info (@:E@ or @:encoding@), the result is an 'RIVar'. For
-- any other shape we still consume every byte but surface just the inner
-- object — the IV metadata is dropped, but the surrounding stream keeps
-- parsing correctly.
getIVar :: Marshal RubyObject -> Marshal RubyObject
getIVar g = marshalLabel "IVar" $ do
  inner <- g
  len <- getFixnum
  ivars <- V.replicateM len (liftM2 (,) g g)
  let maybeEnc = V.foldl' (\acc pair -> acc <|> extractEncoding pair) Nothing ivars
  case (inner, maybeEnc) of
    (RString _, Just enc) -> do
      let result = RIVar (inner, enc)
      writeCache result
      return result
    _ -> return inner
  where
    extractEncoding :: (RubyObject, RubyObject) -> Maybe RubyStringEncoding
    extractEncoding (RSymbol "E", RBool True)         = Just UTF_8
    extractEncoding (RSymbol "E", RBool False)        = Just US_ASCII
    extractEncoding (RSymbol "encoding", RString enc) = Just (toEnc enc)
    extractEncoding _                                 = Nothing

-- | Parses <http://ruby-doc.org/core-2.2.0/Bignum.html Bignum>.
--
-- Wire format: one sign byte (@\'+\'@ or @\'-\'@), then a packed-int count of
-- 16-bit little-endian digits, then that many digits.
getBignum :: Marshal RubyObject
getBignum = marshalLabel "Bignum" $ do
  sign  <- liftMarshal getWord8
  n     <- getFixnum
  bytes <- liftMarshal $ getBytes (n * 2)
  let magnitude = BS.foldr (\b acc -> acc * 256 + fromIntegral b) 0 bytes
      value     = if sign == 0x2D then negate magnitude else magnitude
      result    = RBignum value
  writeCache result
  return result

-- | Parses <http://ruby-doc.org/core-2.2.0/Regexp.html Regexp>.
--
-- Wire format: a raw byte sequence for the pattern, then a single byte of
-- options flags. The pattern's source encoding is typically carried by a
-- surrounding 'RIVar' wrapper, which is parsed independently.
getRegexp :: Marshal RubyObject
getRegexp = marshalLabel "Regexp" $ do
  pat  <- getString
  opts <- liftMarshal getWord8
  let result = RRegexp pat opts
  writeCache result
  return result

-- | Parses Hash with a default value (@}@). Wire format matches a regular
-- Hash followed by one additional object (the default).
getHashWithDefault :: Marshal RubyObject -> Marshal RubyObject
getHashWithDefault g = marshalLabel "HashWithDefault" $ do
  n      <- getFixnum
  pairs  <- V.replicateM n (liftM2 (,) g g)
  def    <- g
  let result = RHashWithDefault pairs def
  writeCache result
  return result

-- | Parses Object (@o@) and Struct (@S@). Both share the wire shape:
-- class symbol, count of pairs, then count many @(symbol, value)@ pairs.
getObjectOrStruct
  :: (BS.ByteString -> V.Vector (RubyObject, RubyObject) -> RubyObject)
  -> String
  -> Marshal RubyObject
  -> Marshal RubyObject
getObjectOrStruct con name g = marshalLabel name $ do
  classSym <- g
  n        <- getFixnum
  pairs    <- V.replicateM n (liftM2 (,) g g)
  let result = case classSym of
        RSymbol cls -> con cls pairs
        _           -> Unsupported
  writeCache result
  return result

-- | Parses a Class/Module name reference (@c@, @m@, @M@). Wire format is a
-- bare byte sequence — note that this is not a Symbol; the bytes are the
-- fully-qualified class or module name.
getNamedRef
  :: (BS.ByteString -> RubyObject)
  -> String
  -> Marshal RubyObject
getNamedRef con name = marshalLabel name $ do
  s <- getString
  let result = con s
  writeCache result
  return result

-- | Parses an object dumped via @_dump@ (@u@). Wire format: class symbol,
-- then a raw byte sequence carrying the user-defined payload.
getUserDef :: Marshal RubyObject -> Marshal RubyObject
getUserDef g = marshalLabel "UserDef" $ do
  classSym <- g
  payload  <- getString
  let result = case classSym of
        RSymbol cls -> RUserDef cls payload
        _           -> Unsupported
  writeCache result
  return result

-- | Parses an object dumped via @marshal_dump@ (@U@) or @_dump_data@ (@d@).
-- Both share the wire shape: class symbol then one arbitrary Marshal object.
getUserMarshalOrData
  :: (BS.ByteString -> RubyObject -> RubyObject)
  -> String
  -> Marshal RubyObject
  -> Marshal RubyObject
getUserMarshalOrData con name g = marshalLabel name $ do
  classSym <- g
  payload  <- g
  let result = case classSym of
        RSymbol cls -> con cls payload
        _           -> Unsupported
  writeCache result
  return result

-- | Parses a wrapper tag — @e@ (object extended with a module) or @C@
-- (object whose class is a user subclass of a builtin). Both read a symbol
-- and then an object, and Ruby does not give the wrapper its own slot in
-- the object table — the inner object owns it. We discard the modifier
-- symbol and pass the inner through unchanged.
getWrapper :: String -> Marshal RubyObject -> Marshal RubyObject
getWrapper name g = marshalLabel name $ do
  _ <- g  -- modifier symbol (module name or subclass name)
  g

-- | Pulls an Instance Variable out of the object cache.
getObjectLink :: Marshal RubyObject
getObjectLink = marshalLabel "ObjectLink" $ do
  index <- getFixnum
  when (index == 0) $ fail $ "invalid object link (index=0)"
  maybeObject <- readObject (index - 1)
  case maybeObject of
    Just x -> return x
    x      -> fail $ "invalid object link (index=" <> show index <> ", target=" <> show x <> ")"

-- | Parses <http://ruby-doc.org/core-2.2.0/String.html String>.
getString :: Marshal BS.ByteString
getString = marshalLabel "RawString" $ do
  n <- getFixnum
  liftMarshal $ getBytes n

-- | Parses <http://ruby-doc.org/core-2.2.0/Symbol.html Symbol>.
getSymbol :: Marshal BS.ByteString
getSymbol = marshalLabel "Symbol" $ do
  x <- getString
  writeCache $ RSymbol x
  return x

-- | Pulls a Symbol out of the symbol cache.
getSymlink :: Marshal BS.ByteString
getSymlink = marshalLabel "Symlink" $ do
  index <- getFixnum
  maybeObject <- readSymbol index
  case maybeObject of
    Just (RSymbol bs) -> return bs
    _                 -> fail "invalid symlink"

--------------------------------------------------------------------
-- Utility functions.

-- | Lift Get into Marshal monad and then label.
liftAndLabel :: String -> Get a -> Marshal a
liftAndLabel x y = liftMarshal $! label x y

-- | Label underlying Get in Marshal monad.
marshalLabel :: String -> Marshal a -> Marshal a
marshalLabel x y = y >>= \y' -> liftMarshal $! label x (return y')

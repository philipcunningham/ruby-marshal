module Data.Ruby.Marshal (
-- * Simple interface to de-serialise Ruby Marshal binary.
  load, loadVerbose,
  module Data.Ruby.Marshal.Get,
  module Data.Ruby.Marshal.Object
) where

import Data.Ruby.Marshal.Get
import Data.Ruby.Marshal.Object

import Data.Serialize.Get (runGet, getWord8)
import Data.Word          (Word8)

import qualified Data.ByteString as BS

-- | De-serialises a subset of Ruby objects serialised with Marshal, Ruby's
-- built-in binary serialisation format.
load :: BS.ByteString
     -- ^ Serialised Ruby object
     -> Either String RubyObject
     -- ^ De-serialisation result.
load x = case loadVerbose x of
   (Right ((_, _), y)) -> Right y
   Left z              -> Left  z

-- | Same as 'load' but also provides the Marshal version number.
loadVerbose :: BS.ByteString -> Either String ((Word8, Word8), RubyObject)
loadVerbose =
  runGet $ do
    major  <- getWord8
    minor  <- getWord8
    object <- getRubyObject
    return ((major, minor), object)

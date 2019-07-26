# ruby-marshal

[![Build Status](https://travis-ci.org/mcfilib/ruby-marshal.svg?branch=master)](https://travis-ci.org/mcfilib/ruby-marshal)

Haskell library to parse a subset of Ruby objects serialised with Marshal.dump.

## Supported Types

- `NilClass`
- `TrueClass | FalseClass`
- `Array`
- `Fixnum`
- `Float`
- `Hash`
- `String`
- `Symbol`

If you would like to add support for another type, please feel free to
create an issue or open a pull request using the guidelines below.

## Usage

### Example

``` haskell
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Ruby.Marshal
import Data.ByteString  (ByteString)
import System.Directory (getCurrentDirectory)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as DM

lookupUserID :: (ByteString, RubyStringEncoding)
             -> RubyObject
             -> Maybe (ByteString, RubyStringEncoding)
lookupUserID key hash = fromRuby hash >>= \cookie -> DM.lookup key cookie

main :: IO ()
main = do
  dir <- getCurrentDirectory
  rbs <- BS.readFile (mconcat [dir, "/test/bin/railsCookie.bin"])
  print $
    case decode rbs of
      Just cookie -> lookupUserID ("user_id", UTF_8) cookie
      Nothing     -> Nothing
```

## Contributing

1. Fork it.
2. Create your feature branch (`git checkout -b my-new-feature`).
3. Commit your changes (`git commit -am 'Add some feature'`).
4. Push to the branch (`git push origin my-new-feature`).
5. Create new Pull Request.

### Contributors

- [@mcfilib](https://github.com/mcfilib)
- [@adinapoli](https://github.com/adinapoli)
- [@kanishka-azimi](https://github.com/kanishka-azimi)
- [@zyla](https://github.com/zyla)
- [@etherz10](https://github.com/etherz10)

## Similar Libraries

- [adjust/gorails](https://github.com/adjust/gorails)
- [instore/node-marshal](https://github.com/instore/node-marshal)
- [mfz/ruby-marshal](https://code.google.com/p/mfz-ruby-marshal)
- [noxyu3m/erlang-ruby-marshal](https://github.com/noxyu3m/erlang-ruby-marshal)

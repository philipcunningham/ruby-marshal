# ruby-marshal

[![Build Status][Build Status Image]][Build Status]
[Build Status Image]: https://travis-ci.org/filib/ruby-marshal.svg?branch=master
[Build Status]: https://travis-ci.org/filib/ruby-marshal
[![Hackage](https://img.shields.io/hackage/v/ruby-marshal.svg)](https://hackage.haskell.org/package/ruby-marshal)

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

import Data.Monoid       (mconcat)
import Data.Ruby.Marshal (decode, RubyObject(..))
import System.Directory  (getCurrentDirectory)

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as DM

key :: RubyObject
key = RIVar (RString "user_id", "UTF-8")

lookupId :: RubyObject -> Maybe RubyObject
lookupId (RHash cookie) = DM.lookup key cookie
lookupId _              = Nothing

main :: IO ()
main = do
  dir <- getCurrentDirectory
  rbs <- BS.readFile (mconcat [dir, "/test/bin/railsCookie.bin"])
  print $ case decode rbs of
    Just cookie -> lookupId cookie
    Nothing     -> Nothing
```

## Contributing

1. Fork it.
2. Create your feature branch (`git checkout -b my-new-feature`).
3. Commit your changes (`git commit -am 'Add some feature'`).
4. Push to the branch (`git push origin my-new-feature`).
5. Create new Pull Request.

### Contributors

- [@filib](https://github.com/filib)
- [@adinapoli](https://github.com/adinapoli)

## Similar Libraries

- [adjust/gorails](https://github.com/adjust/gorails)
- [instore/node-marshal](https://github.com/instore/node-marshal)
- [mfz/ruby-marshal](https://code.google.com/p/mfz-ruby-marshal)
- [noxyu3m/erlang-ruby-marshal](https://github.com/noxyu3m/erlang-ruby-marshal)

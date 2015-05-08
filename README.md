# ruby-marshal

[![Build Status][Build Status Image]][Build Status]
[Build Status Image]: https://travis-ci.org/filib/ruby-marshal.svg?branch=master
[Build Status]: https://travis-ci.org/filib/ruby-marshal

Haskell library to parse a subset of Ruby objects serialised with Marshal.dump.

## Contributing

Check the issue tracker for the Ruby objects that are not yet supported and feel free to implement support for one of them. I'm currently working from [UnmarshalStream.java](https://github.com/jruby/jruby/blob/master/core/src/main/java/org/jruby/runtime/marshal/UnmarshalStream.java) and recommend you do the same.

1. Fork it
2. Create your feature branch (`git checkout -b my-new-feature`)
3. Commit your changes (`git commit -am 'Add some feature'`)
4. Push to the branch (`git push origin my-new-feature`)
5. Create new Pull Request

# 0.1.0

- Separated modules by concern.
- Hid underlying Get monad from consumers to allow us to change the parsing
  library without breaking consumers should a more performant one become
  available.
- Added Rubyable type class to make it easier to go between RubyObject and plain
  Haskell values.
- Replaced Double with Float as per Marshal format.
- Replaced internal representation of Hash with Vector of tuples to simplify
  Rubyable type class and usage for consumers.
- Added more type safety by extracting ADT of all possible Ruby string
  encodings.
- Re-ordered parser to try parsing simpler objects first.
- Used strict State monad instead of non-strict.

# 0.0.1

- Completed fully-functioning parser for a subset of Ruby objects serialised
  with Ruby's Marshal format.

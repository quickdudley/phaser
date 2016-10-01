# phaser
A combinator library for incremental multiple pass parsers.

This library is inspired by the `Text.ParserCombinators.ReadP` module which is part of the base package of the Haskell platform, and also by the conduit library. Except for the `munch` family of functions and `readS_to_p`: most of the functionality of `ReadP` is possible (although fewer convenience functions are currently implemented as of version 0.1.0.0).

Parsers can also be chained à la conduit.

Finally: this library supports detailed error reporting including the position in the file where the parser failed.

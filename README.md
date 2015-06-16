# Hack Assembler in Haskell

`Asmblr` is a small Haskell program that transforms *Hack Assembly* into its machine language representation.

## Features

* Follows a similar structure to the implementation describe by the *Nand to Tetris* book
* Faster then the implementation given with the *Nand to Tetris* course
* Can read from stdin and write to stdout (if requested)
* Uses open-source tools wherever possible

## Building

`Asmblr` requires GHC and cabal to be built and can be done like so:

    $ cabal configure
    $ cabal build

Once built, the `Asmblr` executable will be in the `dist/build/Asmblr` folder, and can be run using cabal, or your shell. The `Asmblr` program can also be installed using `cabal install`. For more information on installing cabal packages and using cabal see [its manual](https://www.haskell.org/cabal/users-guide/).

## Tools

The creation of this software was made possible by the following open source tools and
libraries.

* [Gnu Emacs][], because there is no place like home; and no greater editor!
* [GHC][], for compilation of Haskell code
* [Cabal][], for building the project

## License

This project is licensed under the [GPLv3][]. Please see the LICENSE file for full details.

## Issues

There are some minor issues that can be cleaned up in future releases. Namely, making error output more human readable, and a few minor optimization's.
 
* Explicitly handle if input file dne or is unreadable
* Explicitly handle if output file is un-writable
* Create ParseException type to throw from parser and can be caught in Asmblr.defaultMain
* Add (<?> "error description") throughout parsers to make error output more readable
* Re-factor cInstrDest and cInstrAluOps to remove unneeded use of 'try'
  for optimization's sake
* Write tests so that future changes don't break things (see folder `testsuite`)

[Gnu Emacs]: http://www.gnu.org/software/emacs/
[GPLv3]: https://www.gnu.org/licenses/gpl.html
[GHC]: https://www.haskell.org/ghc/
[Cabal]: https://github.com/haskell/cabal/

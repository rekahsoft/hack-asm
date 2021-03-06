# Hack Assembler in Haskell

* [Introduction](#introduction)
* [Building](#building)
* [Usage](#usage)
* [Tools](#tools)
* [License](#license)
* [Issues](#issues)
* [Related Projects](#related-projects)

## Introduction <a name="introduction"></a>

`Asmblr` is a small Haskell program that transforms *Hack Assembly* into its machine language
representation. The parser itself is made available though the library `hackasm` and can be
included in other projects. To see its documentation run `cabal haddock` and view the
documentation in `dist/doc`.

## Features <a name="features"></a>

* Follows a similar structure to the implementation describe by the *Nand to Tetris* book
* Faster then the implementation given with the *Nand to Tetris* course
* Can read from stdin and write to stdout (if requested)
* Uses open-source tools wherever possible

## Building <a name="building"></a>

`Asmblr` requires GHC and cabal to be built and can be done like so:

    $ cabal configure
    $ cabal build

Once built, the `Asmblr` executable will be in the `dist/build/Asmblr` folder, and can be run
using you shell or cabal (via the command `cabal run Asmblr`). The `Asmblr` program can also be
installed using `cabal install`. For more information on installing cabal packages and using
cabal see [its manual](https://www.haskell.org/cabal/users-guide/).

## Usage <a name="usage"></a>

This project comes with the directory `doc/examples` that contains example *Hack Assembly*
programs used for testing the assembler. To clarify how to run the `Asmblr` binary once built
we will use of of the aforementioned *Hack Assembly* files as an input, in this case
`doc/examples/Pong.asm`.

Running the `Asmblr` using cabal:

    $ cabal run Asmblr -- doc/examples/Pong.asm

Running the `Asmblr` using POSIX shell:

    $ ./dist/build/Asmblr/Asmblr doc/examples/Pong.asm

Both of the previous commands invoke the `Asmblr` on the file `doc/examples/Pong.asm` and
generate a file `Pong.hack` in the current directory. `Asmblr` also supports taking input from
`stdin` as well as writing its output to `stdout` or an arbitrary file given by the user. For
convenience the output of `Asmblr --help` is provided below.

    Usage: Asmblr [OPTION...] file
      -v        --verbose        chatty output on stderr
      -V        --version        show version number
      -h        --help           show program usage
      -o[FILE]  --output[=FILE]  output file or '-' for stdout

## Tools <a name="tools"></a>

The creation of this software was made possible by the following open source tools and
libraries.

* [Gnu Emacs][], because there is no place like home; and no greater editor!
* [GHC][], for compilation of Haskell code
* [Cabal][], for building the project

## License <a name="license"></a>

This project is licensed under the [GPLv3][]. Please see the [LICENSE](../tree/LICENSE) file for full details.

## Issues <a name="issues"></a>

There are some minor issues that can be cleaned up in future releases. Namely, making error
output more human readable, and a few minor optimization's.
 
* Explicitly handle if input file dne or is unreadable
* Explicitly handle if output file is un-writable
* Create ParseException type to throw from parser and can be caught in
  `RekahSoft.HackAsm.CommanLine.defaultMain`
* Add `(<?> "error description")` throughout parsers to make error output more readable
* Write tests so that future changes don't break things (see folder `testsuite`)

If you discover a bug or have an issue with this project, please file a bug using
the [Rekahsoft flyspray powered bug tracker](https://bugs.rekahsoft.ca/index.php?project=8).

## Related Projects <a name="related-projects"></a>

The *Hack Machine Code* generated by this assembler is only useful if a *Hack* machine or
simulator is available. One such simulator is available with the *Nand to Tetris* course.
Alternatively, I've written a
[VHDL implementation of the Hack Platform](http://git.rekahsoft.ca/hack) which can be used to
simulate the *Hack Machine* in order to run the assembly generated by this assembler. Currently
it is not as full featured and user friendly as the simulator given by the *Nand to Tetris*
course, and likely never will be as it's end goal is to be implemented on a FPGA with
verification of the design done via simulation. For more details see
[its README](http://git.rekahsoft.ca/hack/about).

[Gnu Emacs]: http://www.gnu.org/software/emacs/
[GPLv3]: https://www.gnu.org/licenses/gpl.html
[GHC]: https://www.haskell.org/ghc/
[Cabal]: https://github.com/haskell/cabal/

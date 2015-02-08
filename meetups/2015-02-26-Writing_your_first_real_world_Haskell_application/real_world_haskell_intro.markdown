% An introduction to writing real world Haskell applications
% Simon Meier, IBM Research Zurich
% HaskellerZ meetup - February 26th, 2015

Goal of this talk
=================

- provide a blueprint for writing your real-wrold Haskell applications.
- explain how to navigate Haskell libraries and documentation
- not a Haskell syntax and types introduction -- but ask whenever you stumble!

We'll do this by implementing our own `wc` utility.


Setting up our development environment
======================================

- `ghc --version`: should be >= 7.6.1, ideally >= 7.8.4
    - install from [Haskell platform](https://www.haskell.org/platform/) or
      [GHC binary distribution](https://www.haskell.org/ghc/download)
    - install libraries using `cabal` into sandboxes

- `cabal --version`: (should be >= 1.22)
    - by default install some `cabal` from distro
    - make sure that your have `$HOME/.cabal/bin/`
      (`$HOME/Library/Haskell/bin` on OS X) in your $PATH
    - then upgrade to the most recent version `cabal install cabal-install`
    - set `documentation: False` in `$HOME/.cabal/config` to improve build
      times

- decent text-editor that supports block-editing, e.g.,
   [`notepad++`](http://notepad-plus-plus.org/features/column-mode-editing.html),
   [`sublime`](http://www.sublimetext.com/docs/selection),
   [`emacs`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Rectangles.html)
   [`vim`](https://mkrmr.wordpress.com/2010/05/14/vim-tip-visual-block-editing/)
   (my [vim config](https://github.com/meiersi/dotfiles/blob/master/vim/.vimrc))

- search engine shortcuts (`ha` for Hackage, `ho` for Hoogle)

  ![Search engine shortcuts in Chrome](fig/hackage_and_hoogle.png)


Setting up our  project environment
===================================

Create the initial project directory. I typically use a BSD-3 licence and

~~~~
mkdir wc
cd wc
cabal init
~~~~

Resulting directory layout

~~~~
wc/
  LICENSE
  Setup.hs
  wc.cabal
~~~~

Create source directory and initial `Main.hs` file.

~~~~
  mkdir src
  touch src/Main.hs
~~~~


Adapting the initial cabal file
===============================

~~~~
name:                wc
version:             0.1.0
synopsis:            Demo word-count appliation
-- description:
license:             BSD3
license-file:        LICENSE
author:              Simon Meier
maintainer:          simon.y.meier@gmail.com
-- copyright:
category:            System
build-type:          Simple
-- extra-source-files:
cabal-version:       >=1.10

executable wc
  -- other-modules:
  -- other-extensions:
  build-depends:       base >=4.7 && <4.8
  default-language:    Haskell2010

-- Options I have added to the initial config file

  -- Reduce clutter in your top-level directory
  hs-source-dirs:      src

  -- Tell `cabal` where our `main` entry point is.
  main-is: Main.hs

  -- ** -Wall is crucial! **
  -- Your program should always be -Wall clean, as the warnings help you avoid
  -- many stupid mistakes.
  ghc-options:         -Wall
~~~~


Minimal stub for our application
================================

~~~~{.haskell}
main :: IO ()
main = error "wc - main: not yet implemented"
~~~~

- `IO ()` is the type of IO-actions that return a zero-tuple `()` (called
  unit) when run; similar to `void main()` in Java.
- use the `error` function with an *unambiguous* error message to report
  implementation errors

  ~~~~{.haskell}
  -- | Report an implementation error.
  error :: String -> a
  ~~~~


Compiling and running our stub
==============================

All `cabal` commands are to be executed in the top-level project directory.

Create a cabal sandbox in the `wc` directory. Pro tip: use most recent version
of `cabal` and use `cabal --help` to explore options.

~~~~
> cabal sandbox init
Writing a default package environment file to
./wc/cabal.sandbox.config
Creating a new sandbox at
./wc/.cabal-sandbox
~~~~

Install library dependencies into sandbox, build and install our application
into sandbox. We use `-j1` to build with one core and get more detailed output
on `stdout`.

~~~~
> cabal install -j1
Resolving dependencies...
Notice: installing into a sandbox located at
./wc/.cabal-sandbox
Configuring wc-0.1.0...
Building wc-0.1.0...
Preprocessing executable 'wc' for wc-0.1.0...
Installing executable(s) in
./wc/.cabal-sandbox/bin
Installed wc-0.1.0
~~~~

Run our executable.

~~~~
> .cabal-sandbox/bin/wc src/Main.hs
wc: wc - main: not yet implemented
~~~~


Using the REPL to run our stub
==============================

Load project into REPL with dependencies as specified in .cabal file and installed in

~~~~
> cabal repl
Package has never been configured. Configuring with default flags. If this
fails, please run configure manually.
Configuring wc-0.1.0...
Preprocessing executable 'wc' for wc-0.1.0...
GHCi, version 7.8.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
[1 of 1] Compiling Main             ( src/Main.hs, interpreted )

src/Main.hs:1:1: Warning:
    Top-level binding with no type signature: main :: forall t. t
    Ok, modules loaded: Main.
~~~~

Run the programin with command-line arguments.

~~~
> :main src/Main.hs
Exception: wc - main: not yet implemented
~~~

Tip: type `:?` in `ghci` to get a list of all available commands.


The structure of Haskell applications
=====================================

- IO: threads, file reading and writing
- Pure: thread-safe by default, referentially transparent
- Pure total: the gold standard



Implementing `wc -w`
===================

Know your libraries.

- [`base`]() `Int`, `Bool`, lists, control structures, `IO`
- [`bytestring`]() sequences of bytes
- [`text]() sequences of Unicode characters

Really just read/skim their documentation and build a map in your head!

Some useful functions.

~~~~
Prelude.(.)                :: (b -> c) -> (a -> b) -> (a -> c)
Prelude.length             :: [a] -> Int
Prelude.putStrLn           :: String -> IO ()
Data.Text.words            :: Text -> [Text]
System.Environment.getArgs :: IO [String]
~~~~

Adding dependencies: see the
[package  versioning policy](https://wiki.haskell.org/Package_versioning_policy)
  - constraint dependencies to 'x.y.*' for four-digit versions, and
    'x.*' for three digit versions

(Check the demo source code.)



Implementing the remaining functionality
========================================

~~~~
wc [-clmw] [file ...]

-c      The number of bytes in each input file is written to the standard output.
-l      The number of lines in each input file is written to the standard output.
-m      The number of characters in each input file is written to the standard output.
-w      The number of words in each input file is written to the standard output.
~~~~

(Check the demo source code.)

Pro tip: to simplify code-recompile loop: use split-screen layout and
[`ghcid`](https://hackage.haskell.org/package/ghcid).


How to proceed from here
========================

- [Learn You a Haskell](http://learnyouahaskell.com/) for the basics
- lots of structure in type-classes read/skim the
  [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia?)
- lots of information in GHC manual, read/skim it
- libraries to know by heart:
  [safe](https://hackage.haskell.org/package/safe),
  [bytestring](https://hackage.haskell.org/package/bytestring),
  [text](https://hackage.haskell.org/package/text),
  [containers](https://hackage.haskell.org/package/containers),
  [unordered-containers](https://hackage.haskell.org/package/unordered-containers),
  [transformers](https://hackage.haskell.org/package/transformers),
  [either](https://hackage.haskell.org/package/either),
  [mtl](https://hackage.haskell.org/package/mtl),
- libraries to know about:
  lens, aeson, attoparsec, parsec, snap, blaze-html, conduit,
  opt-parse-applicative
- choose a project to work on and ask about problems at our meetups
- use Johan Tibell's
  [styleguide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)
  as an orientation point
- follow <http://www.reddit.com/r/haskell/> and <https://planet.haskell.org/>
  for inspiring pointers


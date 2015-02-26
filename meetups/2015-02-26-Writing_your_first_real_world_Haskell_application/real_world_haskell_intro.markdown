% An introduction to writing real world Haskell applications
% Simon Meier, IBM Research Zurich
% February 26th, 2015

Goal of this talk
=================

- provide a blueprint for writing your own Haskell applications
- explain how to navigate Haskell libraries and documentation
- assumes some knowledge of Haskell syntax: ask if something is unclear!

We'll do this by implementing a clone of the Unix word-count `wc` utility.


Setting up our development environment
======================================

- `ghc --version`: should be ≥ 7.6.1, ideally ≥ 7.8.4
    - install from [Haskell platform](https://www.haskell.org/platform/) or
      [GHC binary distribution](https://www.haskell.org/ghc/download)
    - install libraries using `cabal` (into sandboxes)

- `cabal --version`: should be ≥ 1.22
    - by default install some `cabal` from distro
    - make sure that your have `$HOME/.cabal/bin/`
      (`$HOME/Library/Haskell/bin` on OS X) in your $PATH
    - upgrade to the most recent version: `cabal install cabal-install`
    - set `documentation: False` in `$HOME/.cabal/config` to improve build
      times

- decent text-editor that supports block-editing, e.g.,
   [`notepad++`](http://notepad-plus-plus.org/features/column-mode-editing.html),
   [`sublime`](http://www.sublimetext.com/docs/selection),
   [`emacs`](https://www.gnu.org/software/emacs/manual/html_node/emacs/Rectangles.html)
   [`vim`](https://mkrmr.wordpress.com/2010/05/14/vim-tip-visual-block-editing/)
   (my [vim config](https://github.com/meiersi/dotfiles/blob/master/vim/.vimrc))

- search engine shortcuts (`ha` for Hackage, `ho` for Hoogle)

  ![](fig/hackage_and_hoogle.png)


Setting up our  project environment
===================================

Create the initial project directory for our `wc` clone,

~~~~
mkdir wc
cd wc
~~~~

and populate it.

~~~~
cabal init
~~~~

Resulting directory layout:

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
synopsis:            Demo word-count application
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
  -- Your program should always be -Wall clean, as the warnings
  -- help you avoid many stupid mistakes.
  ghc-options:         -Wall
~~~~


Minimal stub for our application
================================

~~~~{.haskell}
main :: IO ()
main = error "wc - main: not yet implemented"
~~~~

- `IO ()`{.haskell} is the type of IO-actions that return a zero-tuple; called
  unit and written as `()`.

- use the `error` function with an *unambiguous* error message to report
  implementation errors

~~~~{.haskell}
-- | Report an implementation error.
error :: String -> a
~~~~


Compiling and running our stub
==============================

All `cabal` commands are to be executed in the top-level project directory.

Create a cabal sandbox in the `wc` directory. Hint: use most recent version
of `cabal` and use `cabal --help` to explore options.

~~~~
> cabal sandbox init
Writing a default package environment file to
./wc/cabal.sandbox.config
Creating a new sandbox at
./wc/.cabal-sandbox
~~~~

Install library dependencies into sandbox, build and install our application
into sandbox. We use `-j1` to build with one core and get more detailed build
output on `stdout`.

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

Load project into REPL with dependencies as specified in .cabal file and
installed in `.cabal-sandbox/`.

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

Run the program with command-line arguments.

~~~
> :main src/Main.hs
Exception: wc - main: not yet implemented
~~~

Hint: type `:?` in `ghci` to get a list of all available commands.


The structure of Haskell applications
=====================================

![](fig/io_layer.svg)


Implementing `wc -w`
===================

Know your libraries.

- [`base`](https://hackage.haskell.org/package/base):
  `Int`{.haskell}, `Bool`{.haskell}, lists, control structures, `IO`{.haskell}
- [bytestring](https://hackage.haskell.org/package/bytestring):
  sequences of bytes
- [text](https://hackage.haskell.org/package/text):
  sequences of Unicode characters

*Great investment: read/skim their documentation and build a mental map.*

Some useful functions.

~~~~{.haskell}
(.)                        :: (b -> c) -> (a -> b) -> (a -> c)
length                     :: [a] -> Int
putStrLn                   :: String -> IO ()
Data.Text.words            :: Text -> [Text]
System.Environment.getArgs :: IO [String]
~~~~

Adding dependencies: see the
[package  versioning policy](https://wiki.haskell.org/Package_versioning_policy)

  - constrain dependencies to `x.y.*` for four-digit versions, and
    `x.*` for three digit versions

(Check the [demo source
code](https://github.com/meiersi/HaskellerZ/blob/master/meetups/2015-02-26-Writing_your_first_real_world_Haskell_application/wc/src/Main.hs).)



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

Hint: to simplify code-recompile loop: use split-screen layout and
[`ghcid`](https://hackage.haskell.org/package/ghcid).


How to proceed from here
========================

- [Learn You a Haskell](http://learnyouahaskell.com/) for the basics
- lots of structure in type-classes read/skim the
  [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia?)
- lots of information in
  [GHC's user's guide](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/)
- libraries to know by heart:
  [base](https://hackage.haskell.org/package/base),
  [safe](https://hackage.haskell.org/package/safe),
  [bytestring](https://hackage.haskell.org/package/bytestring),
  [text](https://hackage.haskell.org/package/text),
  [containers](https://hackage.haskell.org/package/containers),
  [unordered-containers](https://hackage.haskell.org/package/unordered-containers),
  [transformers](https://hackage.haskell.org/package/transformers),
  [either](https://hackage.haskell.org/package/either),
  [mtl](https://hackage.haskell.org/package/mtl),
- some libraries to know about:
  [lens](https://hackage.haskell.org/package/lens),
  [aeson](https://hackage.haskell.org/package/aeson),
  [attoparsec](https://hackage.haskell.org/package/attoparsec),
  [parsec](https://hackage.haskell.org/package/parsec),
  [snap](https://hackage.haskell.org/package/snap),
  [blaze-html](https://hackage.haskell.org/package/blaze-html),
  [conduit](https://hackage.haskell.org/package/conduit),
  [optparse-applicative](https://hackage.haskell.org/package/optparse-applicative)
- choose a project to work on and ask about problems at our meetups
- use Johan Tibell's
  [styleguide](https://github.com/tibbe/haskell-style-guide/blob/master/haskell-style.md)
  as an orientation point
- follow <http://www.reddit.com/r/haskell/> and <https://planet.haskell.org/>
  for inspiring pointers


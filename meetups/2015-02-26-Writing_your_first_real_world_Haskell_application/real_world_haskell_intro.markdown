% Real World Haskell - An Introduction
% Simon Meier, IBM Research Zurich
% HaskellerZ meetup - February 26th, 2015

Goal of this talk
=================

Provide a blueprint for writing your first Haskell command-line application.

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

-- Options I've added to the initial config file
------------------------------------------------

  -- Reduce clutter in your top-level directory
  hs-source-dirs:      src

  -- We have to tell .cabal where our `main` entry point is.
  main-is: Main.hs

  -- ** -Wall is crucial! **
  -- Your program should always be -Wall clean, as the warnings help you avoid
  -- many stupid mistakes.
  ghc-options:         -Wall
~~~~


Minimal stub for our application
================================

~~~~{.haskell}
main = error "wc - main: not yet implemented"
~~~~

- use the `error` function with an *unambiguous* error message to report
  implementation errors

~~~~{.haskell}
-- | Report an implementation error.
error :: String -> a
~~~~


Compiling our stub
==================

All `cabal` commands are to be executed in the top-level project directory.

Create a cabal sandbox in the `wc` directory. Pro tip: use most recent version
of `cabal`.

~~~~
cabal sandbox init
~~~~

Install library dependencies into sandbox, build and install our application
into sandbox.

~~~~
cabal install
~~~~

Check the outputs of the tools to see whether everything is in good state.





$ ./.cabal-sandbox/bin/wc 
wc: wc - main: not yet implemented

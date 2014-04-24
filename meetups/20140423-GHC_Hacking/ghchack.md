% GHC hacking made simple
% Gergely Risko <<errge@nilcons.com>>
% HaskellerZ meetup - April 24th, 2014

Outline
=======

  - How to fetch and build the sources (both for i386 and amd64)

  - Installing and using the newly baked compiler

  - How to save on edit --- compile --- run time

  - Getting rid of `-ffull-laziness` (also called `-fmake-my-program-leak`)

  - Adding a test to the testsuite

  - Looking into #8488

  - (optionally) Fixing the bug that we found

Getting the source and build instructions
=====

We will use [github.com/nilcons/ceh](https://github.com/nilcons/ceh)
to provide as the stage 0 GHC compiler.

So just follow [github.com/nilcons/ceh/blob/master/README.compile-ghc-with-ceh](https://github.com/nilcons/ceh/blob/master/README.compile-ghc-with-ceh)

~~~ bash
git clone http://git.haskell.org/ghc src
cd src
./sync-all get
/opt/ceh/scripts/ghc-build-shell.pl
  cp mk/build.mk.sample mk/build.mk
  vi mk/build.mk

  cd .. ; mkdir build ; cd build
  lndir ../src; ln -s ../src/.git .)

  perl boot; ./configure --prefix=$HOME/wherever-i-want
  make -j 4

inplace/bin/ghc-stage2 --version
inplace/bin/ghc-stage2 --interactive
~~~

[https://ghc.haskell.org/trac/ghc/wiki/Building/Using](https://ghc.haskell.org/trac/ghc/wiki/Building/Using)

Installation and use
=====
~~~ bash
make install
export PATH=...
cabal ...
ghc ...
~~~

Note that the user compiled binaries are all Nix based too!

Edit --- compile --- run quickly
=====

~~~ bash
vi mk/build.mk (and add stage=2)
make -C compiler fast
make -C ghc 2
inplace/bin/ghc-stage2 ...
make install (still quite fast)
~~~

-ffull-laziness
=====
[trac/ghc/ticket/8457](https://ghc.haskell.org/trac/ghc/ticket/8457)

Let's quickly implement the obvious solution: disable ffull-laziness
when -O, but leave it on when -O2.

Test suite
=====
Let's add a new test to the test suite for the "Annotations from
TemplateHaskell" feature (new in 7.8).

And afterwards look into
[trac/ghc/ticket/8488](https://ghc.haskell.org/trac/ghc/ticket/8488).

[https://ghc.haskell.org/trac/ghc/wiki/Building/RunningTests](https://ghc.haskell.org/trac/ghc/wiki/Building/RunningTests)

Optional: fix "Annotations from TemplateHaskell"

Links
=====
Questions?  Concerns?  Consulting contracts?

[http://nilcons.com/](http://nilcons.com/)

[https://github.com/nilcons/ceh](https://github.com/nilcons/ceh)

[https://ghc.haskell.org/trac/ghc/wiki/Building](https://ghc.haskell.org/trac/ghc/wiki/Building)

[https://ghc.haskell.org/trac/ghc/wiki/Building/Hacking](https://ghc.haskell.org/trac/ghc/wiki/Building/Hacking)

[https://ghc.haskell.org/trac/ghc/wiki/Building/Using](https://ghc.haskell.org/trac/ghc/wiki/Building/Using)

[https://ghc.haskell.org/trac/ghc/wiki/Building/RunningTests](https://ghc.haskell.org/trac/ghc/wiki/Building/RunningTests)

[https://ghc.haskell.org/trac/ghc/wiki/Building/StandardTargets](https://ghc.haskell.org/trac/ghc/wiki/Building/StandardTargets)

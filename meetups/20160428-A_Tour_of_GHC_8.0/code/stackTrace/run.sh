#!/bin/bash

set -o xtrace

ghc-7.10 --make -prof -fprof-auto -rtsopts -outputdir=/tmp plain.hs
./plain +RTS -xc -RTS
rm plain

# ghc-7.10 --make -g -outputdir=/tmp plain.hs

runhaskell-8.0 ./plain.hs

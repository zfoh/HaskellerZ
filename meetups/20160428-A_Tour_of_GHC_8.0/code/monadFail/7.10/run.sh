#!/bin/bash

set -o xtrace

runhaskell-7.10 main-maybe.hs
runhaskell-7.10 main-either.hs
ghc-7.10 -c -outputdir=/tmp -dsuppress-all -ddump-ds compute.hs > compute.ds

#!/bin/bash

set -o xtrace

runhaskell-8.0 main-maybe.hs
runhaskell-8.0 main-either.hs
ghc-8.0 -c -outputdir=/tmp -dsuppress-all -ddump-ds compute.hs > compute.ds

#!/bin/bash

# https://ghc.haskell.org/trac/ghc/wiki/ApplicativeDo

set -o xtrace

for i in 1 2 3 4 5; do
	ghc-8.0                 -O0 -c -outputdir=/tmp -dsuppress-all -ddump-simpl example${i}.hs > example${i}-monadic.ds
	ghc-8.0 -XApplicativeDo -O0 -c -outputdir=/tmp -dsuppress-all -ddump-simpl example${i}.hs > example${i}-applicative.ds
done

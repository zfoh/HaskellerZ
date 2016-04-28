#!/bin/bash

set -o xtrace
set -x


# Without profiling
ghc-8.0          --make -O2 -outputdir=/tmp                            -o measure-nonstrict      measure.hs
ghc-8.0 -XStrict --make -O2 -outputdir=/tmp                            -o measure-strict         measure.hs

# With profiling
ghc-8.0          --make -O2 -outputdir=/tmp -prof -fprof-auto -rtsopts -o measure-nonstrict-prof  measure.hs
ghc-8.0 -XStrict --make -O2 -outputdir=/tmp -prof -fprof-auto -rtsopts -o measure-strict-prof     measure.hs

# Dump ds
ghc-8.0          -c     -O2 -outputdir=/tmp -dsuppress-all -ddump-ds measure.hs > measure-nonstrict.ds    
ghc-8.0 -XStrict -c     -O2 -outputdir=/tmp -dsuppress-all -ddump-ds measure.hs > measure-strict.ds

# Dump stg
ghc-8.0          -c     -O2 -outputdir=/tmp -dsuppress-all -ddump-stg measure.hs > measure-nonstrict.stg
ghc-8.0          -c     -O2 -outputdir=/tmp -dsuppress-all -ddump-stg measure.hs > measure-strict.stg

# Measure time
(time ./measure-nonstrict) 2> measure-nonstrict.time
(time ./measure-strict)    2> measure-strict.time

# Profiling
./measure-nonstrict-prof +RTS -tmeasure-nonstrict.prof
./measure-strict-prof    +RTS -tmeasure-strict.prof

# Cleanup
rm -f measure-nonstrict measure-strict measure-nonstrict-prof measure-strict-prof 

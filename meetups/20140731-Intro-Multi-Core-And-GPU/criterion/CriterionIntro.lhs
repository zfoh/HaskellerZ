% Criterion Introduction

Criterion
---------

  - Library to analyze the performance of a function.

  - Performs multiple evaluations and evaluates detailed statistics.

  - ... and simply has a nice html output.

Using Criterion
---------------

> import Criterion.Main (defaultMain)
> import Criterion

> fib :: Int -> Int
> fib 0 = 0
> fib 1 = 1
> fib n = fib (n-1) + fib (n-2)

> main = defaultMain [
>          bench "fib 10" $ nf fib 10,
>          bench "fib 30" $ nf fib 30
>      ]

Using Criterion
---------------

Compile the code with

    ghc -O2 CriterionIntro.lhs

And run with

    CriterionIntro -o Performance.html

to see a [detailed performance statistic](./Performance.html) for the functions called.

Command line options can be seen with

    CriterionIntro --help



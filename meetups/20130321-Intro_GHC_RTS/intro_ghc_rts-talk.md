% An introduction to GHC's Haskell execution model
% Simon Meier
% HaskellerZ meetup - March 21st, 2013

Goal of this talk
=================

Simplify the understanding of the runtime behaviour of Haskell programs
compiled with GHC. For example,

  - What are the memory requirements of values of the following two types?

    ~~~ {.haskell}
    data List a = Nil | One a (List a)

    data ListInt = NilI | OneI {-# UNPACK #-} !Int ListInt
    ~~~

  - Why is the following function prone to a stack-overflow?
  
    ~~~~ {.haskell}
    sumN :: Int -> Int
    sumN 0 = 0
    sumN n = n + sumN (n - 1)
    ~~~~

Essentially, this talk provides an introduction to
[GHC's RTS
commentary](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts),
which guides through [GHC's RTS sources](https://github.com/ghc/ghc/tree/master/rts).


Memory layout of (heap) objects
===============================

GHC uses the same basic layout for *every* object in the RTS. 

Basic closure layout                          Basic info table layout
--------------------------------------------  ----------------------------------------------
![Basic closure layout](fig/heap-object.png)  ![Basic info table layout](fig/basic-itbl.png)
--------------------------------------------  ----------------------------------------------

See also: [GHC commentary](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects)

Example: a list of Int's
===========================

~~~~ {.haskell}
data List a = Nil | One a (List a)

ex1 :: List Int
ex1 = One 1 (One 2 (One 3 Nil))
~~~~

TODO: Image


Example: a list of unboxed Int's
================================

~~~~ {.haskell}
data ListInt = NilI | OneI {-# UNPACK #-} !Int ListInt

ex1 :: ListInt
ex1 = OneI 1 (OneI 2 (OneI 3 NilI))
~~~~

TODO: Image


Haskell evaluation
==================

Case distinctions are the fuel for the execution of Haskell code.

~~~~ {.haskell}
case x of _ -> e
~~~~

ensures that the value denoted by the variable `x` is in weak head normal
form, i.e., has no outermost function application.

TODO: Add examples from stack overflow question.



Example: summing a list of Int's (I)
====================================

~~~~ {.haskell}
sumList :: List Int -> Int
sumList Nil        = 0
sumList (One x xs) = x + sumList xs

test :: Int
test = sumList (One 1 (One 2 (One 3 Nil)))
~~~~~

High-level outline of reducing `test` to WHNF:

~~~~ {.haskell}
   test
~> sumList (One 1 (One 2 (One 3 Nil)))
~> 1 + sumList (One 2 (One 3 Nil))
~> 1 + (2 + sumList (One 3 Nil))
~> 1 + (2 + (3 + (sumList Nil)))
~> 1 + (2 + (3 + 0))
~> 1 + (2 + 3)
~> 1 + 5
~> 6
~~~~


Example: summing a list of Int's (II)
=====================================

~~~~ {.haskell}
sumList :: List Int -> Int
sumList xs = case xs of
               Nil      -> 0
               One x xs -> x + sumList xs

test :: Int
test = sumList ex1

ex1 = One 1 ex1_1
ex1_1 = One 2 ex1_2
ex1_2 = One 3 ex1_3
ex1_3 = Nil
~~~~

Reducing `test` to WHNF with explicit stack management.

~~~~ {.haskell}
   test                                    <| []
~> sumList ex_1                            <| []
~> case ex1 of Nil      -> 0
               One x xs -> x + sumList xs  <| []
~> ex1                                     <| [case ?? of ...]
~> One 1 ex1_1                             <| [case ?? of ...]
~> case One 1 ex1_1 of ...                 <| []
~> (+) 1 (sumList ex1_1)
~> sumList ex1_1                           <| [(+) 1 ??]
~> case ex1_1 of ...                       <| [(+) 1 ??]
~> ex1_1                                   <| [case ?? of ..., (+) 1 ??]
~> One 2 ex1_2                             <| [case ?? of ..., (+) 1 ??]
~> case One 2 ex1_2 of ...                 <| [(+) 1 ??]
~> (+) 2 (sumList ex1_2)                   <| [(+) 1 ??]
~> sumList ex1_2                           <| [(+) 2 ??, (+) 1 ??]
~> .... ~>
~> 0                                       <| [(+) 3 ??, (+) 2 ??, (+) 1 ??]
~> (+) 0 3                                 <| [(+) 2 ??, (+) 1 ??]
~> 3                                       <| [(+) 2 ??, (+) 1 ??]
~> (+) 2 3                                 <| [(+) 1 ??]
~> 5                                       <| [(+) 1 ??]
~> (+) 1 5                                 <| []
~> 6                                       <| []
~~~~


Evaluation costs
================

When executing a Haskell program, time is spent on

- allocating new heap objects
- executing primitive operations
- case distinctions
- calling functions

The Spineless Tagless G-machine (STG machine) makes these costs explicit.

See also TODO: Link to SPJ journal paper.


Overview: GHC's compilation pipeline
====================================

![](fig/HscPipe2.png)

See also [GHC commentary]( TODO


Example: core-prep of `sumList`
===============================

~~~~ {.haskell}
data List a = Nil | One a (List a)

sumList :: List Int -> Int
sumList Nil        = 0
sumList (One x xs) = x + sumList xs

ex1 :: List Int
ex1 = One 1 (One 2 (One 3 Nil))

test = sumList ex1
~~~~

Compiled with `ghc -O2 -dsuppress-all -ddump-prep List.hs`
yields

~~~~ {.haskell}
==================== CorePrep ====================

ex6 = I# 1
ex5 = I# 2
ex4 = I# 3
ex3 = One ex4 (Nil)
ex2 = One ex5 ex3
ex1 = One ex6 ex2

Rec {
$wsumList =
  \ w_sca ->
    case w_sca of _ {
      Nil -> 0;
      One x_sce xs_sch ->
        case x_sce of _ { I# x1_scj ->
        case $wsumList xs_sch of ww_sck { __DEFAULT -> +# x1_scj ww_sck }
        }
    }
end Rec }

sumList =
  \ w_scm ->
    case $wsumList w_scm of ww_sco { __DEFAULT -> I# ww_sco }

test = case $wsumList ex1 of ww_scq { __DEFAULT -> I# ww_scq }

Nil = Nil
One = \ @ a_aaf eta_B2 eta_B1 -> One eta_B2 eta_B1
~~~~


Example: map over a list of Int's
=================================

~~~~ {.haskell}
mapList :: (a -> b) -> List a -> List b
mapList f Nil        = Nil
mapList f (One x xs) = One (f x) (mapList f xs)

foo :: Int -> Int
foo x = x + 0xDeadBeef

test :: List Int
test = mapList foo (One 1 (One 2 (One 3 Nil)))
~~~~

Compiled with `ghc -O2 -dsuppress-all -ddump-prep List.hs`
yields

~~~~ {.haskell}
==================== CorePrep ====================
Rec {
mapList =
  \ @ a_abc @ b_abd f_sct ds_sco ->
    case ds_sco of _ {
      Nil -> Nil;
      One x_scs xs_scv ->
        let {
          sat_scE = mapList f_sct xs_scv } in
        let {
          sat_scF = f_sct x_scs } in
        One sat_scF sat_scE
    }
end Rec }

foo =
  \ x_scy ->
    case x_scy of _ { I# x1_scB ->
    case +# x1_scB 3735928559 of sat_scG { __DEFAULT -> I# sat_scG }
    }

test6 = I# 1
test5 = I# 2
test4 = I# 3
test3 = One test4 (Nil)
test2 = One test5 test3
test1 = One test6 test2
test = mapList foo test1

Nil = Nil
One = \ @ a_aab eta_B2 eta_B1 -> One eta_B2 eta_B1
~~~~


Example: memory layout during execution (I)
===========================================

~~~~ {.haskell}
mapList f Nil        = Nil
mapList f (One x xs) = One (f x) (mapList f xs)

foo :: Int -> Int
foo x = x + 0xDeadBeef
~~~~

Memory layout *before* reducing `mapList foo (One 1 (One 2 Nil))` to WHNF.

TODO: Image

Example: memory layout during execution (II)
============================================

~~~~ {.haskell}
mapList f Nil        = Nil
mapList f (One x xs) = One (f x) (mapList f xs)

foo :: Int -> Int
foo x = x + 0xDeadBeef
~~~~

Memory layout *after* reducing `mapList foo (One 1 (One 2 Nil))` to WHNF.

TODO: Image


Common closure types
====================

  - Constructors: `CONSTR`, `CONSTR_p_n`, `CONSTR_STATIC`, ...
  - Functions: `FUN`, `FUN_p_n`, `FUN_STATIC`, ...
  - Thunks: `THUNK`, `THUNK_p_n`, `THUNK_SELECTOR`, ...
  - (Partial) applications: `PAP`, `AP`
  - Indirections: `IND`, `IND_OLD_GEN`, ...
  - MVars, Black holes, Byte-code objects, Thread State Objects, ...


For more information see 
[GHC commentary](http://hackage.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects#Typesofobject):



Exploiting our newly gained knowledge (I)
=========================================

What implementation is faster?

~~~~{.haskell}
sumN :: Int -> Int
sumN 0 = 0
sumN n = n + sumN (n - 1)

sumNAcc :: Int -> Int
sumNAcc =
    go 0
  where
    go acc 0 = acc
    go acc n = go (n + acc) (n -1)
~~~~


Exploiting our newly gained knowledge (II)
==========================================

**-> Your example here <-**



Thanks...
=========

...for listening. Further references,

 - stock hardware
 - fast curry
 - pointer tagging
 - GHC commentary
 - GHC sources




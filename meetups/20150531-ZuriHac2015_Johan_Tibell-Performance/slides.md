% Writing Production Quality Code
% Johan Tibell
% May 31, 2015


## Goal

The goal of this talk is to give you a set of guidelines for writing
production quality code.

These guidelines are based on what we *actually do* in our core
libraries (bytestring, text, containers, attoparsec, etc).

The goal is to avoid performance problems "by design", not by being
"very careful".


## Outline

* Reasoning about space usage

* Reasoning about laziness

* Inlining

* Reading Core


## Reasoning about space usage

Knowing how GHC represents values in memory is useful because

* it allows us to approximate memory usage, and

* it allows us to count the number of indirections, which affect
  cache behavior.


## Memory usage for data constructors

Rule of thumb: a constructor uses one word for a header, and one
word for each field.  So e.g.

~~~~ {.haskell}
data Uno = Uno a
data Due = Due a b
~~~~

an `Uno` takes 2 words, and a `Due` takes 3.

* Exception: a constructor with no fields (like `Nothing` or `True`)
  takes no space, as it's shared among all uses.


## Memory layout

Here's how GHC represents the list `[1,2]` in memory:

<p>![](list12.png)</p>

* Each box represents one machine word

* Arrows represent pointers

* Each constructor has one word overhead for e.g. GC information


## Refresher: unboxed types

GHC defines a number of _unboxed_ types.  These typically represent
primitive machine types.

* By convention, the names of these types end with a
  `#`.

* Most unboxed types take one word (except
  e.g. `Double#` on 32-bit machines)

* Values of unboxed types cannot be thunks.

* The basic types are defined in terms unboxed types e.g.

~~~~ {.haskell}
data Int = I# Int#
~~~~

* We call types such as `Int` _boxed_ types


## Poll

How many machine words is needed to store a value of this data type:

~~~~ {.haskell}
data IntPair = IP Int Int
~~~~

* 3?

* 5?

* 7?

* 9?

Tip: Draw a boxes-and-arrows diagram.


## IntPair memory layout

<p>![](intpair.png)</p>

So an `IntPair` value takes 7 words.


## Refresher: unpacking

GHC gives us some control over data representation via the
`UNPACK` pragma.

* The pragma unpacks the contents of a constructor into the
  field of another constructor, removing one level of indirection
  and one constructor header.

* Only fields that are strict, monomorphic, and single-constructor
  can be unpacked.

The pragma is added just before the bang pattern:

~~~~ {.haskell}
data Foo = Foo {-# UNPACK #-} !SomeType
~~~~

GHC 7 and later will warn if an `UNPACK` pragma cannot be used because
it fails the use constraint.


## Unpacking example

~~~~ {.haskell}
data IntPair = IP !Int !Int
~~~~

<p>![](intpair.png)</p>

~~~~ {.haskell}
data IntPair = IP {-# UNPACK #-} !Int
                  {-# UNPACK #-} !Int
~~~~

<p>![](intpair-unpacked.png)</p>


## A structural comparison with C

By reference:

~~~~ {.haskell}
-- Haskell
data A = A !Int
~~~~

~~~~ {.c}
// C
struct A {
  int *a;
};
~~~~

By value:

~~~~ {.haskell}
-- Haskell
data A = A {-# UNPACK #-} !Int
~~~~

~~~~ {.c}
// C
struct A {
  int a;
};
~~~~

If you can figure out which C representation you want, you can figure
out which Haskell representation you want.

## Benefits of unpacking

When the pragma applies, it offers the following benefits:

* Reduced memory usage (4 words saved in the case of `IntPair`)

* Removes indirection

Caveat: There are (rare) cases where unpacking hurts performance
e.g. if the value is passed to a non-strict function, as it needs to
be reboxed.

**Unpacking is one of the most important optimizations available to
 us.**


## Compiler support

Starting with GHC 7.10, small (pointer-sized* or less) strict fields
are unpacked automatically.

\* Applies to `Double` even on 32-bit architectures.


## Reasoning about laziness

A function application is only evaluated if its result is needed,
therefore:

* One of the function's right-hand sides will be evaluated.

* Any expression whose value is required to decide which RHS to
  evaluate, must be evaluated.

These two properties allow us to use "back-to-front" analysis (known
as demand/strictness analysis) to figure which arguments a function is
strict in.


## Example

~~~~ {.haskell}
max :: Int -> Int -> Int
max x y
    | x >= y = x
    | x <  y = y
~~~~

* To pick one of the two RHSs, we must evaluate `x > y`.

* Therefore we must evaluate _both_ `x` and `y`.

* Therefore `max` is strict in both `x` and `y`.


## Poll

~~~~ {.haskell}
data Tree = Leaf | Node Int Tree Tree

insert :: Int -> Tree -> Tree
insert x Leaf   = Node x Leaf Leaf
insert x (Node y l r)
    | x < y     = Node y (insert x l) r
    | x > y     = Node y l (insert x r)
    | otherwise = Node x l r
~~~~

Which argument(s) is `insert` strict in?

* None

* 1st

* 2nd

* Both


## Solution

Only the second, as inserting into an empty tree can be done without
comparing the value being inserted.  For example, this expression

~~~~ {.haskell}
insert (1 `div` 0) Leaf
~~~~

does not raise a division-by-zero expression but

~~~~ {.haskell}
insert (1 `div` 0) (Node 2 Leaf Leaf)
~~~~

does.


## Strict data types cannot contain thunks

Given a function `f :: ... -> T` where `T` is a type with only strict
fields*, whose value are types containing only strict fields, and so
forth, cannot contain any thunks.

~~~~ {.haskell}
data T1 = C1 !Int  -- Exactly 2 words
data T2 = C2 Int   -- 2-inf words
~~~~

Therefore, `f` cannot leak space after the evaluation of `f` has
finished.

Using strict fields is thus a way to prevent space leaks, without
sprinkling bangs all over the definition of `f`.

\* of non-function type, because closures can retain data.


## Guideline 1: data types should be strict by default

* Allows a more compact data representation.

* Avoids many space leaks by construction.

* Can be more cache-friendly to create.


## In practice: strict can be more cache-friendly

Example: `Data.Map`

~~~~ {.haskell}
data Map k a = Tip
             | Bin {-# UNPACK #-} !Size !k a
                   !(Map k a) !(Map k a)
~~~~

  (Note the bang on the `Map k a` fields.)

* Most container types have a strict spine.

* Strict spines cause more work to be done up-front (e.g. on
  `insert`), when the data structure is in cache, rather than later
  (e.g. on the next `lookup`.)

* Does not always apply (e.g. when representing streams and other
  infitinte structures.)


## Guideline 2: use strict data types in accumulators

If you're using a composite accumulator (e.g. a pair), make sure it has
strict fields.

Allocates on each iteration:

~~~~ {.haskell}
mean :: [Double] -> Double
mean xs = s / n
  where (s, n) = foldl' (\ (s, n) x -> (s+x, n+1)) (0, 0) xs
~~~~

Doesn't allocate on each iteration:

~~~~ {.haskell}
data StrictPair a b = SP !a !b

mean2 :: [Double] -> Double
mean2 xs = s / n
  where SP s n = foldl' (\ (SP s n) x -> SP (s+x) (n+1)) (SP 0 0) xs
~~~~

Haskell makes it cheap to create throwaway data types like
`StrictPair`: one line of code.


## Guideline 3: use strict returns in monadic code

`return` often wraps the value in some kind of (lazy) box.  This is an
example of a hidden lazy data type in our code.  For example, assuming
we're in a state monad:

~~~~ {.haskell}
return $ x + y
~~~~

creates a thunk.  We most likely want:

~~~~ {.haskell}
return $! x + y
~~~~

Just use `$!` by default.


## In practice: beware of the lazy base case

Functions that would otherwise be strict might be made lazy by the
"base case":

~~~~ {.haskell}
data Tree = Leaf
          | Bin Key !Value !Tree !Tree

insert :: Key -> Value -> Tree
insert k v Leaf = Bin k v Leaf Leaf  -- lazy in @k@
insert k v (Bin k' v' l r)
   | k < k'    = ...
   | otherwise = ...
~~~~

Since GHC does good things to strict arguments, we should make the
base case strict, unless the extra laziness is useful:

~~~~ {.haskell}
insert !k v Leaf = Bin k v Leaf Leaf  -- strict in @k@
~~~~

In this case GHC might unbox the key, making all those comparisons
cheaper.

## Guideline 4: force expressions before wrapping them in lazy data types

We don't control all data types in our program.

* Many standard data types are lazy (e.g. `Maybe`, `Either`, tuples).

* This means that it's easy to be lazier than you intend by wrapping
  an expression in such a value:

~~~~ {.haskell}
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just $ x / y  -- creates thunk
~~~~

* Force the value (e.g. using `$!`) before wrapping it in the
  constructor.


## INLINE all the things!?!

* **Don't.** GHC typically does a good job inlining code on its own.

* We probably inline too much in core libraries, out of paranoia.

* We should however *make it possible* for GHC to inline (see next
  slide).


## Guideline 5: Add wrappers to recursive functions

* GHC does not inline recursive functions:

~~~~ {.haskell}
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
~~~~

* **If** you want to inline a recursive function, use a non-recursive
  wrapper like so:

~~~~ {.haskell}
map :: (a -> b) -> [a] -> [b]
map f = go
  where
    go []     = []
    go (x:xs) = f x : go xs
~~~~


## Inlining HOFs avoids indirect calls

* Calling an unknown function (e.g. a function that's passed as an
  argument) is more expensive than calling a known function.  Such
  *indirect* calls appear in higher-order functions:

~~~~ {.haskell}
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

g xs = map (+1) xs  -- map is recursive => not inlined
~~~~

* If we use the non-recursive wrapper from the last slide, GHC will
  likely inline `map` into `g`.

* It's useful to Inline HOFs if the higher-order argument is used a
  lot (e.g. in `map`, but not in `Data.Map.insertWith`). Sometimes GHC
  gets this wrong (check the Core) and you can use a manual pragma to
  help it.


## Guideline 6: Use INLINABLE

Use `INLINABLE` to remove overhead from type classes.

* Despite its name, it works quite differently from `INLINE`.

* `INLINABLE` gives us a way to do call-site specialization of type
  class parameters.
  
Given

~~~~ {.haskell}
module M1 where
    f :: Num a => a -> a -> a
    f x y = ...
    {-# INLINABLE f #-}

module M2 where
    main = print $ f (1 :: Int) 2
~~~~

GHC will create a copy of `f` at the call site, specialized to `Int`.


## GHC Core

* GHC uses an intermediate language, called "Core," as its internal
  representation during several compilation stages
* Core resembles a subset of Haskell
* The compiler performs many of its optimizations by repeatedly
  rewriting the Core code


## Why knowing how to read Core is important

Reading the generated Core lets you answer many questions, for
example:

* When are expressions evaluated?
* Is this function argument accessed via an indirection?
* Did my function get inlined?


## Convincing GHC to show us the Core

Given this "program"

~~~~ {.haskell}
module Sum where

import Prelude hiding (sum)

sum :: [Int] -> Int
sum []     = 0
sum (x:xs) = x + sum xs
~~~~

we can get GHC to output the Core by adding the `-ddump-simpl` flag

~~~~
$ ghc -O -ddump-simpl -dsuppress-module-prefixes \
    -dsuppress-idinfo Sum.hs -fforce-recomp
~~~~


## Reading Core: a guide

* Unless you use the `-dsuppress-*` flags,

    - all names are fully qualified (e.g.`GHC.Types.Int` instead of
      just `Int`) and
    - there's lots of meta information about the function (strictness,
      types, etc.)

* Lots of the names are generated by GHC (e.g. `w_sgJ`).

Note: The Core syntax changes slightly with new compiler releases.


## Tips for reading Core

Three tips for reading Core:

* Open and edit it in your favorite editor to simplify it (e.g. rename
  variables to something sensible).
* Use the ghc-core package on Hackage
* Use the GHC Core major mode in Emacs (ships with haskell-mode)


## Core for the sum function

~~~~ {.haskell}
Rec {
$wsum :: [Int] -> Int#
$wsum =
  \ (w_sxC :: [Int]) ->
    case w_sxC of _ {
      [] -> 0;
      : x_amZ xs_an0 ->
        case x_amZ of _ { I# x1_ax0 ->
        case $wsum xs_an0 of ww_sxF { __DEFAULT -> +# x1_ax0 ww_sxF }
        }
    }
end Rec }

sum :: [Int] -> Int
sum =
  \ (w_sxC :: [Int]) ->
    case $wsum w_sxC of ww_sxF { __DEFAULT -> I# ww_sxF }
~~~~


## Core for sum explained

* Convention: A variable name that ends with # stands for an unboxed
  value.
* GHC has split `sum` into two parts: a wrapper, `sum`, and a worker,
  `$wsum`.
* The worker returns an unboxed integer, which the wrapper wraps in an
  `I#` constructor.
* `+#` is addition for unboxed integers (i.e. a single assembler
  instruction).
* GHC has added a note that `sum` should be inlined.


## What can we learn from this Core?

~~~~ {.haskell}
Rec {
$wsum :: [Int] -> Int#
$wsum =
  \ (w_sxC :: [Int]) ->
    case w_sxC of _ {
      [] -> 0;
      : x_amZ xs_an0 ->
        case x_amZ of _ { I# x1_ax0 ->
        case $wsum xs_an0 of ww_sxF { __DEFAULT -> +# x1_ax0 ww_sxF }
        }
    }
end Rec }
~~~~

The worker is not tail recursive, as it performs an addition after
calling itself recursively.

This means that it will use more stack.

We should probably rewrite it to use an accumulator parameter.

## Summary: how to write production quality code

* Think about memory layout. Use back-of-the-envelope calculations to
  figure out how much memory your data types will take (e.g. if you
  want to store lots of data in a map).

* Use strict fields by default.

* Know the limited number of cases (e.g. accumulator recursion) where
  you need to use an explicit bang pattern.

* Don't go overboard with inlining.

* Learn to read Core, it's useful and fun!

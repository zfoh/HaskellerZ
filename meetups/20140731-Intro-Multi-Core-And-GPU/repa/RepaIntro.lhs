% Repa Introduction

Repa
----

  - Library for parallel array computations on multi-core machines.

  - For shared memory.

  - Multidimensional unboxed (and boxed) arrays.

Using Repa
----------

The basic array type in Repa is given by

~~~ haskell
data Array rep sh e
~~~

> {-# LANGUAGE TypeOperators #-}
> import Data.Array.Repa as R
> import Data.Array.Repa.Index as I
> import qualified Data.Array.Repa.Repr.Vector as V
> import Data.Array.Repa.Repr.Unboxed
> import Data.Array.Repa.Eval

The type tag `rep` is a type hint for the array type that is present. The two most
important types are `U` for unboxed data and `D` for delayed data, i.e. data that
has to be calculated (`V` for boxed values). 

We can create a `Shape sh` with the constructor `Z`

> shape' :: (Z :. Int) :. Int
> shape' = Z :. 2 :. 3

We can also abbreviate this with

> shape :: DIM2
> shape = ix2 2 3

Using Repa
----------

We create an array from a list with

> m :: Array U DIM2 Int
> m = fromListUnboxed shape [0..5]

The `Shape` typeclass is not only used to determine the bounds of the arrays but
also to access the elements.

  - Elements are indexed from `0` :-)
  - Elements are in row major order :-/

Element access can be performed with the `(!)` operator.

~~~ haskell
m ! ((Z :. 0) :. 0)
~~~

Using Repa
----------

The extent of the matrix can be obtained with

> tmp   = extent m

The number of dimensions with

> tmp'  = rank tmp

And the size (number of elements) with

> tmp'' = size tmp

Conversion from and to unboxed `Vector`s with `toUnboxed` and `fromUnboxed`.

Using Repa
----------

> m' :: Array D DIM2 Int
> m' = R.zipWith (+) (R.map (^2) m) m

`m'` cannot be shown as it is a delayed array. We can turn it into a concrete
array using `computeS`.

> m'' :: Array U DIM2 Int
> m'' = computeS m'

Commands to create a represented array from a calculation

~~~ haskell
computeS :: (Load r1 sh e, Target r2 e) => Array r1 sh e -> Array r2 sh e
computeP :: (Load r1 sh e, Target r2 e, Source r2 e, Monad m) => Array r1 sh e -> m (Array r2 sh e)
~~~

Slices
------

~~~ haskell
slice m (Any :. (1::Int) :. All)
slice m (Any :. All :. (1::Int))
~~~

~~~ haskell
slice m (Any :. (1::Int))
~~~

is equivalent to

~~~ haskell
slice m (Any :. All :. (1::Int))
~~~

`Any` cannot be omitted.

Extending
---------

A matrix can be extended along multiple dimensions:

~~~ haskell
extend (Any :. (4::Int) :. All :. All) m
extend (Any :. All :. (4::Int) :. All) m
extend (Any :. All :. All :. (4::Int)) m
~~~

Example: Matrix Matrix Multiplication
-------------------------------------

$$ z_{ik} = \sum_{j} x_{ij} \, y_{jk} $$

$$ z_{ik} = \sum_{j} \hat{x}_{ikj} \, \hat{y}_{ikj} $$

where $\hat{x}_{ikj} = x_{ij}$ and $\hat{y}_{ikj} = y_{jk}$

~~~ haskell
mmult :: Array U DIM2 Double -> Array U DIM2 Double -> Array U DIM2 Double
mmult a b = R.sumS $ R.zipWith (*) a' b''
  where
    a'  = extend (Any :. All :. n :. All) a
    b'  = transpose b
    b'' = extend (Any :. m :. All :. All) b'
~~~

Traverse
--------

Generic array traversal with

~~~ haskell
traverse :: (Source r a, Shape sh', Shape sh) => 
    Array r sh a             -- input array 
 -> (sh -> sh')              -- calculation of new extent
 -> ((sh -> a) -> sh' -> b)  -- function that given a reader of the original array and
                             -- a position in the new array calculates a value
 -> Array D sh' b            -- new array :-)
~~~


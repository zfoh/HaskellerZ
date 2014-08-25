% Accelerate Introduction

GPU Computing with Accelerate
-----------------------------

  - GPU with many cores.

  - Restricted or expensive control flow.

  - Only limited number of instruction decoders.

  - Restricted GPU memory size when compared to CPU memory.

  - Limited bandwidth for transfer of data from CPU to GPU and back.

> {-# LANGUAGE TypeOperators #-}
> import Data.Array.Accelerate as A
> import Data.Array.Accelerate.Interpreter as I

Using Accelerate
----------------

The basic array type in Accelerate is given by

~~~ haskell
data Array sh e
~~~

  - In contrast to the design of Repa 3 there is not tag for the representation.
  
  - Same data type as for Repa 1

  - `Shapes` are constructed using the same data types as for Repa (`Z`, ...).

> n :: Int
> n = 2
>
> shape :: DIM1
> shape = Z :. n
>
> x :: Array DIM1 Int
> x = fromList (Z :. n) [1..]
>
> y :: Array DIM1 Int
> y = fromList (Z :. n) [2..]

Using arrays on the GPU
-----------------------

~~~ haskell
use :: Arrays arrays => arrays -> Acc arrays -- "CPU to GPU"
run :: Arrays arrays => Acc arrays -> arrays -- "GPU to CPU"
~~~

> tmp = I.run $ A.zipWith (*) (use x) (use y)

~~~ haskell
unit :: Elt e => Exp e -> Acc (Scalar e)
the  :: Elt e => Acc (Scalar e) -> Exp e

constant :: Elt t => t -> Exp t
~~~

Creating Arrays in Accelerate
-----------------------------

~~~ haskell
fill :: (Elt e, Shape sh) => Exp sh -> Exp e -> Acc (Array sh e)
enumFromN :: (Elt e, Shape sh, IsNum e) => Exp sh -> Exp e -> Acc (Array sh e)
enumFromStepN :: (Elt e, Shape sh, IsNum e) => Exp sh -> Exp e -> Exp e -> Acc (Array sh e)
generate :: (Elt a, Shape ix) => Exp ix -> (Exp ix -> Exp a) -> Acc (Array ix a)
~~~

Working with `Exp`:

~~~ haskell
lift :: Lift c e => e -> c (Plain e)
unlift :: Unlift c e => c (Plain e) -> e
~~~

`unlift` can be used to take apart structures values inside an `Exp`, i.e.

> f = unlift :: Exp (Int,Int) -> (Exp Int,Exp Int)
> 
> tmp' = lift (constant 1, constant 1) :: Exp (Int,Int)
>
> scalarProd x y = A.sum $ A.zipWith (*) x y


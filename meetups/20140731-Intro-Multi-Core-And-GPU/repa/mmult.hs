{-# LANGUAGE TypeOperators #-}
import Data.Array.Repa as R
import Data.Array.Repa.Index as I
import Data.Array.Repa.Algorithms.Randomish as W
import Data.Array.Repa.Eval             as R
import Data.Array.Repa.Unsafe           as R
import Control.Monad.ST.Strict

m = 500 :: Int
k = 500 :: Int
n = 500 :: Int

a :: Array U DIM2 Double
a = randomishDoubleArray (ix2 m k) 0.0 1.0 1234

b :: Array U DIM2 Double
b = randomishDoubleArray (ix2 k n) 0.0 1.0 5678

mmult :: Array U DIM2 Double -> Array U DIM2 Double -> Array U DIM2 Double
mmult a b = R.sumS $ R.zipWith (*) a' b''
  where
    a'  = extend (Any :. All :. n :. All) a
    b'  = transpose b
    b'' = extend (Any :. m :. All :. All) b'

main = do
  putStrLn $ show $ sumAllS $ mmult a b

-- Implementation from Repa.Algorithms
row :: DIM2 -> Int
row (Z :. r :. _) = r
{-# INLINE row #-}

col :: DIM2 -> Int
col (Z :. _ :. c) = c
{-# INLINE col #-}

mmultS :: Array U DIM2 Double -> Array U DIM2 Double -> Array U DIM2 Double
mmultS arr brr= runST $
   do   trr     <- R.now $ transpose brr
        let (Z :. h1  :. _)  = extent arr
        let (Z :. _   :. w2) = extent brr
        return $ computeS 
         $ fromFunction (Z :. h1 :. w2)
         $ \ix   -> R.sumAllS 
                  $ R.zipWith (*)
                        (unsafeSlice arr (Any :. (row ix) :. All))
                        (unsafeSlice trr (Any :. (col ix) :. All))

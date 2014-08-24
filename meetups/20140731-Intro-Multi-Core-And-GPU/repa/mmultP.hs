{-# LANGUAGE TypeOperators #-}
import Data.Array.Repa as R
import Data.Array.Repa.Index as I
import Data.Array.Repa.Algorithms.Randomish as W
import Data.Array.Repa.Eval             as R
import Control.Monad.ST.Strict

m = 500 :: Int
k = 500 :: Int
n = 500 :: Int

a :: Array U DIM2 Double
a = randomishDoubleArray (ix2 m k) 0.0 1.0 1234

b :: Array U DIM2 Double
b = randomishDoubleArray (ix2 k n) 0.0 1.0 5678

mmult :: Array U DIM2 Double -> Array U DIM2 Double -> Array U DIM2 Double
mmult a b = runST $ R.sumP $ R.zipWith (*) a' b''
  where
    a'  = extend (Any :. All :. n :. All) a
    b'  = transpose b
    b'' = extend (Any :. m :. All :. All) b'

main = do
  putStrLn $ show $ sumAllS $ mmult a b

{-# LANGUAGE TypeOperators #-}
import Data.Array.Accelerate as A
import Data.Array.Accelerate.CUDA as C

n :: Int
n = 10000000000

shape :: DIM1
shape = Z :. n

x :: Acc (Array DIM1 Float)
x = enumFromStepN (constant (Z :. n)) (constant 1) (constant 1)

y :: Acc (Array DIM1 Float)
y = enumFromStepN (constant (Z :. n)) (constant $ Prelude.fromIntegral n) (constant (-1))

scalarProd x y = A.sum $ A.zipWith (*) x y

main = do
  putStrLn $ show $ C.run $ scalarProd x y

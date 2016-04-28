module Compute where

data D = A Int | B Int

f :: Monad m => Int -> m Int
f x = do
  A y <- g x
  return (y+1)

g :: Monad m => Int -> m D
g x = return $ B x

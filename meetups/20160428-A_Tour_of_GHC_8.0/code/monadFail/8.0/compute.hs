{-# LANGUAGE MonadFailDesugaring #-}
module Compute where

import Control.Monad.Fail

data D = A Int | B Int

f :: MonadFail m => Int -> m Int
f x = do
  A y <- g x
  return (y+1)

g :: Monad m => Int -> m D
g x = return $ B x

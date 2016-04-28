module Example5 (f5) where

f5 a b c d = do
  x1 <- a
  x2 <- b
  x3 <- c x1
  x4 <- d
  return (x2,x3,x4)

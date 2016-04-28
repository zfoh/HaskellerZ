module Example3 (f3, g3) where

f3 m = do { x <- m True; y <- m x; return (x || y) }

g3 m = m True >>= \x -> fmap (\y -> x || y) (m x)

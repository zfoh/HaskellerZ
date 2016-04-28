module Example1 (f1, g1) where

f1 m = do { x <- m; return (not x) }

g1 m = fmap (\x -> not x) m

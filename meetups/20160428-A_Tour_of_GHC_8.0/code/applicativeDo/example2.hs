module Example2 (f2, g2) where

f2 m = do { x <- m 'a'; y <- m 'b'; return (x || y) }

g2 m = (\x y -> x || y) <$> m 'a' <*> m 'b'

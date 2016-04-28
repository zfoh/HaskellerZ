module Example4 (f4) where

f4 a b m = do {x <- a; y <- b; m (x, y)}

--g4 a b m = 

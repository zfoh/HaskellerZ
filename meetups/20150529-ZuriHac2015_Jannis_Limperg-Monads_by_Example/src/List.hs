module List where

import Prelude hiding (Monad, (>>=), return)
import Monad

-- concatMap f = concat . map f

instance Monad [] where
    -- (>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>= f = concatMap f xs
    return x = [x]

test :: [(Int, Int)]
test = [1 ..  5] >>= \x ->
       [x .. 10] >>= \y ->
       return (x, y)

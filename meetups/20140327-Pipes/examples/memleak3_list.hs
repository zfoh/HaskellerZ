-- Exact analogue of memleak3.hs, but with lists instead of pipes.
-- See there for behavior.

import Control.Monad

numbers :: () -> [Int]
-- {-# NOINLINE numbers #-}
numbers () = go 0
  where
    go n = n : go (n+1)

main :: IO ()
main = do
  forM_ (numbers ()) print
  forM_ (numbers ()) print

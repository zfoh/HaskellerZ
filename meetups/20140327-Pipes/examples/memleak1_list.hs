-- Exact analogue of memleak1.hs, but with lists instead of pipes.
-- "Leaks" no matter how it's compiled.

import Control.Monad

numbers :: [Int]
numbers = go 0
  where
    go n = n : go (n+1)

main :: IO ()
main = do
  forM_ numbers print
  forM_ numbers print

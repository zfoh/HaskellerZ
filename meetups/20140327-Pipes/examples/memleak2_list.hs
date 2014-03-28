-- Exact analogue of memleak2.hs with lists instead of pipes.
--
-- Fundamentally correct code, shouldn't leak
-- Doesn't leak with -O0
-- _Does_ leak with -O
-- Doesn't leak with -O -fno-full-laziness

import Control.Monad

numbersFrom :: Int -> [Int]
numbersFrom = go
  where
    go n = n : go (n+1)

main :: IO ()
main = do
  let n = 42
  forM_ (numbersFrom n) print
  forM_ (numbersFrom n) print

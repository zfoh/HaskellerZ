-- Fundamentally correct code, shouldn't leak
-- Doesn't leak with -O0
-- _Does_ leak with -O
-- Doesn't leak with -O -fno-full-laziness

import Pipes
import Pipes.Core

numbersFrom :: Int -> Producer Int IO ()
numbersFrom = go
  where
    go n = do yield n
              go (n+1)

main :: IO ()
main = do
  let n = 42
  runEffect $ numbersFrom n //> lift . print
  runEffect $ numbersFrom n //> lift . print

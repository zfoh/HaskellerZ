-- Fundamentally correct code, shouldn't leak.
--
-- Doesn't leak with -O0
-- _Does_ leak with -O
-- _Does_ leak with -O -fno-full-laziness
-- _Does_ leak with -O -fno-strictness
-- Doesn't leak with -O -fno-full-laziness -fno-strictness
-- _Does_ leak with -O and 'numbers' marked NOINLINE
-- Doesn't leak with -O -fno-full-laziness and 'numbers' marked NOINLINE

import Pipes
import Pipes.Core

numbers :: () -> Producer Int IO ()
-- {-# NOINLINE numbers #-}
numbers () = go 0
  where
    go n = yield n >> go (n+1)

main :: IO ()
main = do
  runEffect $ numbers () //> lift . print
  runEffect $ numbers () //> lift . print

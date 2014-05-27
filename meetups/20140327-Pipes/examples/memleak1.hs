-- Leaks for fundamental reasons, no matter how it's compiled.

import Pipes
import Pipes.Core
import qualified Pipes.Prelude as P

numbers :: Producer Int IO ()
numbers = go 0
  where
    go n = do yield n
              go (n+1)

printSome :: IO ()
printSome =
  runEffect $ numbers >-> P.take 4 //> lift . print

main :: IO ()
main = do
  runEffect $ numbers //> lift . print
  runEffect $ numbers //> lift . print

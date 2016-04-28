import GHC.Stack
import Prelude hiding (head)


f ::
  HasCallStack =>
  Int -> Int
f x = error $ "Error: " ++ show x

g ::
  HasCallStack =>
  Int -> Int
g = withFrozenCallStack f

main = print $ g 23

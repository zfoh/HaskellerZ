import Control.Monad
import Pipes
import Pipes.Core
import qualified Pipes.Prelude as P

chunkify :: Monad m => Int -> Pipe a [a] m r
chunkify n = forever $ do
  xs <- replicateM n await
  yield xs

main :: IO ()
main = do
  putStrLn "hello"
  -- print $ numList 10000
  runEffect $ numbers >-> chunkify 10000 >-> P.take 1 //> lift . print . last


numbers :: Producer Int IO ()
numbers = forM_ [0..] yield


myReplicateM :: Monad m => Int -> m a -> m [a]
myReplicateM n op = go n
  where
    go 0 = return []
    go k = do
      x <- op
      xs <- go (k-1)
      return (x : xs)

accReplicateM :: Monad m => Int -> m a -> m [a]
accReplicateM n op = go n []
  where
    go 0 acc = return $ reverse acc
    go k acc = do
      x <- op
      go (k-1) (x : acc)

numList :: Int -> [Int]
numList 0 = []
numList n = numList (n-1) ++ [n]

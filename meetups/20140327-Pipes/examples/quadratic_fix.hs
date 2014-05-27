import Control.Monad
import Control.Monad.Codensity
import Pipes
import Pipes.Core
import qualified Pipes.Prelude as P

chunkify :: Monad m => Int -> Pipe a [a] m r
chunkify n = forever $ do
  xs <- lowerCodensity $ replicateM n (lift await)
  yield xs

main :: IO ()
main = do
  putStrLn "hello"
  runEffect $ numbers >-> chunkify 10000 >-> P.take 1 //> lift . print . last


numbers :: Producer Int IO ()
numbers = forM_ [0..] yield

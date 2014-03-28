import Control.Monad (forever, forM_)
import Pipes
import Prelude hiding (take)

main = runEffect $ streamList [0..] >-> take 10 >-> printAll

streamList :: Monad m => [a] -> Producer a m ()
streamList xs = forM_ xs yield

take :: Monad m => Int -> Pipe a a m ()
take 0 = return ()
take n = do
  x <- await
  yield x
  take (n-1)

printAll :: Show a => Consumer a IO r
printAll = forever $ do
  x <- await
  lift $ print x

{-# LANGUAGE Rank2Types #-}

import Control.Concurrent (threadDelay)
import Control.Monad
import Pipes
import Pipes.Core
import Pipes.Internal
import qualified Pipes.Prelude as P

loopPipe :: Monad m => Maybe a -> Proxy () a () a m r -> Effect' m r
loopPipe = go
  where
    go _ (Pure r) = Pure r
    go v (M op) = M $ go v `liftM` op
    go (Just x) (Request _ cPipe) = go Nothing (cPipe x)
    go Nothing (Request _ _) = error "loopPipe: request with no data in the buffer"
    go Nothing (Respond x cPipe) = go (Just x) (cPipe ())
    go (Just _) (Respond _ _) = error "loopPipe: respond with data already in the buffer"



pipe :: Pipe Int Int IO r
pipe = forever $ do
  x <- await
  lift $ do print x
            threadDelay 200000
  yield $ x + 1



useD :: Monad m => (b -> m ()) -> b -> Proxy a' a () b m ()
useD op x = lift (op x) >> yield x

execD :: Monad m => m () -> b -> Proxy a' a () b m ()
execD = useD . const

main :: IO ()
main = do
  runEffect $ loopPipe (Just 1) $ pipe
  runEffect $ loopPipe (Just 0) $
    P.map (+1) >-> P.take 10 //> useD print //> execD (threadDelay 200000)

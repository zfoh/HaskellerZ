

data Step a m r = Yield (a, Pipe a m r)
                | Await (a -> Pipe a m r)
                | Done r

data Pipe a m r = Pipe { stepPipe :: m (Step a m r) }

instance Functor (Pipe a m)
instance Monad m => Monad (Pipe a m)

(>->) :: Monad m => Pipe a m r -> Pipe a m r -> Pipe a m r
pUp >-> (Pipe down) = Pipe $ down >>= go
  where
    go (Done r) = return $ Done r
    go (Yield (x, pDown')) = return $ Yield (x, pUp >-> pDown')
    go (Await cDown) = stepPipe $ pUp >>-> cDown

(>>->) :: Monad m => Pipe a m r -> (a -> Pipe a m r) -> Pipe a m r
(Pipe up) >>-> cDown = Pipe $ up >>= go
  where
    go (Done r) = return $ Done r
    go (Await cUp) = return $ Await $ \x -> cUp x >>-> cDown
    go (Yield (x, pUp')) = stepPipe $ pUp' >-> cDown x

runPipe :: Monad m => Pipe a m r -> m r
runPipe (Pipe p) = p >>= go
  where
    go (Done r) = return r
    go (Yield _) = fail "A closed pipe failed"
    go (Await _) = fail "A closed pipe awaited"

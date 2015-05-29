module Monad where

import Prelude hiding (Monad, (>>=), return)

class Monad m where
    (>>=)  :: m a -> (a -> m b) -> m b
    return :: a -> m a

(>=>) :: (Monad m) => (a -> m b) -> (b -> m c) -> (a -> m c)
f >=> g = \a -> f a >>= g

ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ f = f

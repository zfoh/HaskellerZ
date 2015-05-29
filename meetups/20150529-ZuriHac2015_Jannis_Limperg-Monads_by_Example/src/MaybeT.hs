{-# LANGUAGE RebindableSyntax #-}
-- This module was not used during the talk.

module MaybeT where

import Prelude hiding (Monad, (>>=), return)
import Monad

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

instance (Monad m) => Monad (MaybeT m) where
    -- (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    --       ~= m (Maybe a) -> (a -> m (Maybe b)) -> m (Maybe b)
    (MaybeT m) >>= f = MaybeT $ do
      x <- m
      case x of
        Nothing -> return Nothing
        Just x' -> case f x' of
                     MaybeT x'' -> x''

    -- return :: a -> MaybeT m a
    --        ~= a -> m (Maybe a)
    return = MaybeT . return . Just

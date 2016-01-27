{-# LANGUAGE LambdaCase #-}

-- | Extended version of "Control.Monad.Except" that provides
-- extra utility functions.
module Control.Monad.Except.Extended
    (
      module Control.Monad.Except

      -- * Convenience functions for converting between error types
    , errorIfNothing

      -- * Handling and running except monad transformers
    , handleExceptT
    , rethrowExceptT
    , handleExcept
    , rethrowExcept
    ) where

import           Control.Monad.Except


-- | Throw an error if the argument is 'Nothing' and extract the value
-- otherwise.
errorIfNothing :: MonadError e m => e -> Maybe a -> m a
errorIfNothing err = maybe (throwError err) return

-- | Run an 'ExceptT' computation and immediatly handle the returned errors.
handleExceptT
  :: Monad m
  => (e -> m b)
  -> ExceptT e m b
  -> m b
handleExceptT handler act =
    runExceptT act >>= \case
      Left e -> handler e
      Right x -> return x

-- | Run an 'Except' computation and immediatly handle the returned errors.
handleExcept
  :: Monad m
  => (e -> m b)
  -> Except e b
  -> m b
handleExcept handler act =
    case runExcept act of
      Left e -> handler e
      Right x -> return x

-- | Run an 'ExceptT' computation and immediatly rethrow errors adapted to the
-- error type of the outer monad.
rethrowExceptT
  :: MonadError e' m
  => (e -> e')
  -> ExceptT e m b
  -> m b
rethrowExceptT = handleExceptT . (throwError .)

-- | Run an 'Except' computation and immediatly rethrow errors adapted to the
-- error type of the outer monad.
rethrowExcept
  :: MonadError e' m
  => (e -> e')
  -> Except e a
  -> m a
rethrowExcept = handleExcept . (throwError .)

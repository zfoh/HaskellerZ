-- | Extended version of "Control.Monad.Trans.Either" that provides a common
-- set of utility functions.
module Control.Monad.Trans.Either.Extended
    (
      module Control.Monad.Trans.Either

      -- * Handling 'Bool's
    , errorIfFalse
    , errorIfFalseM

      -- * Handling 'Maybe's
    , errorIfNothing
    , errorIfNothingM

      -- * Mapping over errors
    , mapError
    , mapErrorM

    ) where

import           Control.Monad
import           Control.Monad.Trans.Either


-- | Throw an error if the argument is 'False'.
errorIfFalse :: Monad m => e -> Bool -> EitherT e m ()
errorIfFalse err b = if b then return () else left err

-- | Throw an error if the argument returns 'False'.
errorIfFalseM :: Monad m => e -> EitherT e m Bool -> EitherT e m ()
errorIfFalseM err m = m >>= errorIfFalse err

-- | Throw an error if the argument is 'Nothing' and extract the value
-- otherwise.
errorIfNothing :: Monad m => e -> Maybe a -> EitherT e m a
errorIfNothing err = maybe (left err) return

-- | Throw an error if the argument returns 'Nothing' and extract the returned
-- value otherwise.
errorIfNothingM :: Monad m => e -> EitherT e m (Maybe a) -> EitherT e m a
errorIfNothingM err m = m >>= errorIfNothing err

-- | Map over the error of an inner computation. Useful to inject errors of
-- pure subcomputations into the error type used in the caller.
mapError :: Monad m => (e' -> e) -> Either e' a -> EitherT e m a
mapError f = EitherT . return . (either (Left . f) Right)

-- | Map over the error of an inner computation. Useful to inject errors of
-- subcomputations into the error type used in the caller.
mapErrorM :: Monad m => (e' -> e) -> EitherT e' m a -> EitherT e m a
mapErrorM f = EitherT . liftM (either (Left . f) Right) . runEitherT



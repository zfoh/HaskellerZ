{-# LANGUAGE LambdaCase #-}
-- | An extended version of the 'Bound' module from the 'bound' library, which
-- provides support for working with expressions with bound variables.
--
-- This moodule is intended to be imported qualified as follows:
--
-- > import qualified Bound.Extended as B
--
module Bound.Extended
  ( module Bound
  , module Bound.Var
  , instantiateM
  ) where


import Bound
import Bound.Var

import Elevence.Prelude


-- | Instantiate the bound variables of a 'Scope' with side-effects. We use
-- this to track errors stemming from invalid references to bound variables.
instantiateM
    :: (Applicative g, Monad f, Traversable f)
    => (b -> g (f a)) -> Scope b f a -> g (f a)
instantiateM inst e =
    fmap join $ traverse (\case B i -> inst i; F v -> pure v) (unscope e)

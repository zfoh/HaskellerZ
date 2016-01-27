{-# LANGUAGE NoImplicitPrelude #-}
-- | An extended version of "Data.Either" providing additional utilities when
-- using it to handle errors in pure code.
--
-- We do not expose these functions in the "Elevence.Prelude" because there
-- are going to be equivalent versions of them in
-- "Control.Monad.Trans.Either.Extended".
module Data.Either.Extended
    ( module Data.Either

    , errorIfFalse
    , errorIfNothing
    ) where


import Data.Either

import Elevence.Prelude


------------------------------------------------------------------------------
-- Additional error management utilitites
------------------------------------------------------------------------------

-- | Throw an error if the argument is 'False'.
errorIfFalse :: e -> Bool -> Either e ()
errorIfFalse err b = if b then Right () else Left err

-- | Throw an error if the argument is 'Nothing' and extract the value
-- otherwise.
errorIfNothing :: e -> Maybe a -> Either e a
errorIfNothing err = maybe (Left err) Right


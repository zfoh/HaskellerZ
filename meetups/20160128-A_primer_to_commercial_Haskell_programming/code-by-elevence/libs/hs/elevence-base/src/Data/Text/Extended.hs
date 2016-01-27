{-# LANGUAGE NoImplicitPrelude #-}
-- | An extended version of "Data.Text", which provides a slightly more
-- extensive export lists and missing orphan instances.
module Data.Text.Extended
  (
    module Data.Text

  , show

    -- ** Lenses
  , packed
  , unpacked

  ) where

import           Data.Text.Lens (packed, unpacked)
import           Data.Text

import           Elevence.Prelude hiding (show)
import qualified Elevence.Prelude


-- | 'Show' a value and pack it into a strict 'Text' value.
show :: Show a => a -> Text
show = pack . Elevence.Prelude.show


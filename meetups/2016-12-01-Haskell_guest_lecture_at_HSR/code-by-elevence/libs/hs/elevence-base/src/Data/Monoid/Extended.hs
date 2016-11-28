-- | An extended version of "Data.Monoid" providing 'when_' and 'unless_'.
module Data.Monoid.Extended
  ( module Data.Monoid
  , when_
  , unless_
  ) where


import           Data.Monoid

import           Prelude


-- | Like 'when' but with a 'Monoid' constraint only.
when_ :: Monoid m => Bool -> m -> m
when_ True  m = m
when_ False _ = mempty

-- | Like 'unless' but with a 'Monoid' constraint only.
unless_ :: Monoid m => Bool -> m -> m
unless_ False m = m
unless_ True  _ = mempty

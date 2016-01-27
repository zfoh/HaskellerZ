{-# LANGUAGE CPP #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | An extended version of "Data.IntSet", which provides a slightly more
-- extensive export lists

module Data.IntSet.Extended
  (
    module Data.IntSet

  , symmetricDifference
  , toggle
  ) where

import           Data.IntSet

import           Elevence.Prelude hiding (union)

-- | Symmetric difference of two sets.
symmetricDifference :: IntSet -> IntSet -> IntSet
symmetricDifference s1 s2 = union s1 s2 `difference` intersection s1 s2

-- | Symmetric difference of a set and a singleton set.  This is
-- exactly the behavior of toggling.
toggle :: Int -> IntSet -> IntSet
toggle item s = symmetricDifference (singleton item) s

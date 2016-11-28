{-# LANGUAGE NoImplicitPrelude #-}
-- | An extended version of "Data.Set", which provides a slightly more
-- extensive export lists
module Data.Set.Extended
  (
    module Data.Set

  , symmetricDifference
  , toggle
  , setForM
  , setForM_
  ) where

import           Data.Set

import           Elevence.Prelude hiding (union)

-- | Symmetric difference of two sets.
symmetricDifference :: Ord a => Set a -> Set a -> Set a
symmetricDifference s1 s2 = union s1 s2 `difference` intersection s1 s2

-- | Symmetric difference of a set and a singleton set.  This is
-- exactly the behavior of toggling.
toggle :: Ord a => a -> Set a -> Set a
toggle item s = symmetricDifference (singleton item) s

-- | Iterate over the elements of a set in ascending order, apply a monad
-- action on each and create a new set from the resulting elements.
setForM :: (Ord b, Monad m) => Set a -> (a -> m b) -> m (Set b)
setForM s f = fromList <$> mapM f (toAscList s)

-- | Like 'setForM' but without returning a result.
setForM_ :: (Ord b, Monad m) => Set a -> (a -> m b) -> m ()
setForM_ s f = mapM_ f (toAscList s)

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | An IntMap that by default does not reuse keys, so on insertion we
-- give you the key which is unique as far as you don't use the
-- truncate method, so you can delete already existing keys and even
-- holes are possible in the middle of the current range.  Indexing by
-- default starts from 0.
module Data.GrowingIntMap
  ( GrowingIntMap

    -- * Construction
  , Data.GrowingIntMap.empty
  , Data.GrowingIntMap.emptyWithStartingIndex
  , Data.GrowingIntMap.insert
  , Data.GrowingIntMap.delete
  , Data.GrowingIntMap.truncate
  , Data.GrowingIntMap.highest
  , fromList

    -- * Query
  , foldMapWithKey
  ) where


import           Control.Lens

import qualified Data.IntMap.Strict  as IMS

import           Elevence.Prelude


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | An IntMap whose keys are never reused over the whole evolution of
-- the map.
--
-- NOTE that this only holds provided that the 'Int' does not overflow due to
-- enough calls to 'push' or 'mappend'.
data GrowingIntMap a = GrowingIntMap
    { __gimNextKey :: !Int
    , _gimEntries  :: !(IMS.IntMap a)
    } deriving (Functor, Foldable, Traversable)


-- lenses
---------

makeLenses ''GrowingIntMap


-- instances
------------

makeInstances ''GrowingIntMap


-- lens-library instances

type instance Index   (GrowingIntMap a) = Int
type instance IxValue (GrowingIntMap a) = a

instance Ixed (GrowingIntMap a) where
    ix k = gimEntries . ix k

instance At (GrowingIntMap a) where
    at k = gimEntries . at k


------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------

-- | Generates an empty map.
empty :: GrowingIntMap a
empty = GrowingIntMap 0 mempty

-- | Generates an empty map with the specified starting index.
emptyWithStartingIndex :: Int -> GrowingIntMap a
emptyWithStartingIndex n = GrowingIntMap n mempty

-- | Pushes an item onto the back of the map.
insert :: a -> GrowingIntMap a -> (Int, GrowingIntMap a)
insert x (GrowingIntMap k es) = 
    let !k' = k + 1
    in  (k, GrowingIntMap k' (IMS.insert k x es))

-- | Deletes an item from the map.
delete :: Int -> GrowingIntMap a -> GrowingIntMap a
delete k (GrowingIntMap n es) = GrowingIntMap n (IMS.delete k es)

-- | Return the highest key currently stored in the queue.
highest :: GrowingIntMap a -> Int
highest (GrowingIntMap k _es) = k - 1

-- | Drop everything that is equal or above the specified index and
-- make it so that the next index is set back to the specified index.
truncate :: Int -> GrowingIntMap a -> GrowingIntMap a
truncate k' (GrowingIntMap n es) =
    GrowingIntMap k (fst $ IMS.split k es)
  where
    k = min (max k' 0) n

-- | Converts a list into a map.
fromList :: [a] -> GrowingIntMap a
fromList xs = GrowingIntMap
              (Elevence.Prelude.length xs)
              (IMS.fromList (zip [0..] xs))

-- | Calls foldMapWithKey on the internal IntMap.
foldMapWithKey :: Monoid m => (Int -> a -> m) -> GrowingIntMap a -> m
foldMapWithKey f (GrowingIntMap _ es) = IMS.foldMapWithKey f es

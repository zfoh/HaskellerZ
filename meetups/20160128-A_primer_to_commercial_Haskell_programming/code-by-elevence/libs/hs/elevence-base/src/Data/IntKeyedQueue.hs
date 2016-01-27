{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | A queue whose entries are identified by 'Int' keys that remain stable
-- over the whole evolution of the queue.
--
-- NOTE (SM): in principle we could offer a dequeue interface with analogous
-- stability guarantees by also tracking the lowest key that has been handed
-- out.
module Data.IntKeyedQueue
  ( IntKeyedQueue

    -- * Construction
  , Data.IntKeyedQueue.empty
  , takeFront
  , pushBack
  , fromList

    -- * Queries
  , Data.IntKeyedQueue.null
  , Data.IntKeyedQueue.length
  , first
  , entries
  , highest
  ) where


import           Control.Lens

import qualified Data.IntMap.Strict  as IMS
import qualified Data.Semigroup      as Sg

import           Elevence.Prelude


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | A queue whose entries are identified by 'Int' keys that remain stable
-- over the whole evolution of the queue.
--
-- NOTE that this only holds provided that the 'Int' does not overflow due to
-- enough calls to 'push' or 'mappend'.
data IntKeyedQueue a = IntKeyedQueue
    { __ikdNextKey :: !Int
    , _ikdEntries  :: !(IMS.IntMap a)
    } deriving (Functor, Foldable, Traversable)


-- lenses
---------

makeLenses ''IntKeyedQueue


-- instances
------------

makeInstances ''IntKeyedQueue

-- | The entries of the right queue are shifted such that the won't collide
-- with any existing entry of the left queue.
instance Sg.Semigroup (IntKeyedQueue a) where
    IntKeyedQueue n0 es0 <> IntKeyedQueue n1 es1 =
        IntKeyedQueue (n0 + n1) (es0 <> (IMS.mapKeys (n0 +) es1))

instance Monoid (IntKeyedQueue a) where
    mempty  = IntKeyedQueue 0 mempty
    mappend = (Sg.<>)


-- lens-library instances

type instance Index   (IntKeyedQueue a) = Int
type instance IxValue (IntKeyedQueue a) = a

instance Ixed (IntKeyedQueue a) where
    ix k = ikdEntries . ix k

instance At (IntKeyedQueue a) where
    at k = ikdEntries . at k


------------------------------------------------------------------------------
-- Functions
------------------------------------------------------------------------------

-- | Generates an empty queue.
empty :: IntKeyedQueue a
empty = mempty

-- | Returns 'True' if this queue is empty.
null :: IntKeyedQueue a -> Bool
null = IMS.null . _ikdEntries

-- | Returns the number of elements in this queue.
length :: IntKeyedQueue a -> Int
length = IMS.size . _ikdEntries

-- | Returns the item on the front of the queue.
first :: IntKeyedQueue a -> Maybe a
first = fmap fst . popFront

-- | Returns the first n items from the front of the queue, in the order
-- they would be popped.
takeFront :: Int -> IntKeyedQueue a -> [a]
takeFront n = take n . IMS.elems . _ikdEntries

-- | Pushes an item onto the back of the queue.
pushBack :: a -> IntKeyedQueue a -> (Int, IntKeyedQueue a)
pushBack x (IntKeyedQueue k es) = 
    let !k' = k + 1
    in  (k, IntKeyedQueue k' (IMS.insert k x es))

-- | Pops an item from the front of the queue.
popFront :: IntKeyedQueue a -> Maybe (a, IntKeyedQueue a)
popFront (IntKeyedQueue k es0) = do
    (x, es) <- IMS.minView es0
    return $ (x, IntKeyedQueue k es)

-- | Converts a list into a queue.
fromList :: [a] -> IntKeyedQueue a
fromList xs = IntKeyedQueue (Elevence.Prelude.length xs) (IMS.fromList (zip [0..] xs))

-- | Return a the map containing all queue entries.
entries :: IntKeyedQueue a -> IMS.IntMap a
entries = _ikdEntries

-- | Return the highest key currently stored in the queue.
highest :: IntKeyedQueue a -> Int
highest ikq = __ikdNextKey ikq - 1

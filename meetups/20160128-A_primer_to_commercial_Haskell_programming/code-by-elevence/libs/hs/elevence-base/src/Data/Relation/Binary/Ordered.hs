{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Simple relations represented internally using ordered map and sets.
-- This module is intended to be imported qualified as follows.
--
-- > import qualified Data.Relation.Binary.Ordered as RBO
--
module Data.Relation.Binary.Ordered
  (
    -- * Relations
    Relation

    -- ** Construction
  , fromList
  , singleton
  , insert

  , invert
  , compose
  , transitiveClosure

    -- ** Destruction
  , size
  , domain
  , range
  , toList

  , universe

    -- ** Membership
  , member
  , image

    -- ** Properties
  , symmetric
  , reflexive
  , transitive
  ) where


import qualified Data.Aeson       as Aeson
import qualified Data.Semigroup   as Sg
import qualified Data.Set         as S
import qualified Data.Map.Strict  as MS

import           Elevence.Prelude hiding (insert)


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

newtype Relation a b = Relation { unRelation :: MS.Map a (S.Set b) }
    deriving (Eq, Ord, Show)


-- instances
------------

instance (Ord a, Ord b) => Sg.Semigroup (Relation a b) where
    Relation r1 <> Relation r2 = Relation (MS.unionWith S.union r1 r2)

instance (Ord a, Ord b) => Monoid (Relation a b) where
    mempty  = Relation MS.empty
    mappend = (Sg.<>)

instance Foldable (Relation a) where
    foldMap f = foldMap (foldMap f) . unRelation

instance
       (Ord a, Ord b, Aeson.ToJSON a, Aeson.ToJSON b)
    => Aeson.ToJSON (Relation a b)
  where
    toJSON = Aeson.toJSON . toList

instance
       (Ord a, Ord b, Aeson.FromJSON a, Aeson.FromJSON b)
    => Aeson.FromJSON (Relation a b)
  where
    parseJSON = fmap fromList . Aeson.parseJSON


------------------------------------------------------------------------------
-- Construction
------------------------------------------------------------------------------

-- | /O(log n)/. Insert a tuple into the relation.
insert :: (Ord a, Ord b) => (a, b) -> Relation a b -> Relation a b
insert (a, b) (Relation r) =
    Relation $ MS.insertWith S.union a (S.singleton b) r

-- | /O(1)/. Construct a singleton relation.
singleton :: (a, b) -> Relation a b
singleton (a, b) = Relation $ MS.singleton a (S.singleton b)

-- | /O(n * log n)/. Convert a list to a relation.
fromList :: (Ord a, Ord b) => [(a,b)] -> Relation a b
fromList =
    Relation . foldl' ins MS.empty
  where
    ins m (k, v) = MS.insertWith S.union k (S.singleton v) m

-- | /O('size' r * log ('size' r)). Invert the relation.
invert :: (Ord a, Ord b) => Relation a b -> Relation b a
invert = fromList . map swap . toList

-- | /O(size(r1) * size(r2)). Compose two relations.
compose
    :: (Ord a, Ord b, Ord c) => Relation a b -> Relation b c -> Relation a c
compose (Relation r1) (Relation r2) =
    Relation $ MS.map (S.unions . map compose1 . S.toList) r1
  where
    compose1 y = MS.findWithDefault S.empty y r2

-- | /O(size(result)^2 * log(size(result))^2)/. Compute the transitive closure
-- of a relation.
transitiveClosure :: Ord a => Relation a a -> Relation a a
transitiveClosure rel0
  | size rel0 == size rel = rel
  | otherwise             = transitiveClosure rel
  where
    rel = rel0 <> (rel0 `compose` rel0)


------------------------------------------------------------------------------
-- Destruction
------------------------------------------------------------------------------

-- | /O('size' r)/. Convert a relation to a list.
toList :: Relation a b -> [(a,b)]
toList r = [ (x, y) | (x, ys) <- MS.toList (unRelation r), y <- S.toList ys ]

-- | /O('S.size' ('domain' r))/ Compute the number of tuples in the relation.
--
-- >>> size r == length (toList r)
--
size :: Relation a b -> Int
size = MS.foldl' (\i s -> i + S.size s) 0 . unRelation

-- | /O(log n)/. @'image' x@ computes the set @{ y | (x,y) `'member'` r}@.
image :: Ord a => a -> Relation a b -> S.Set b
image x = MS.findWithDefault S.empty x . unRelation

-- | /O(log n)/. @'member' (x, y) r@ returns true iff @(x, y) `elem` toList
-- xs@.
member :: (Ord a, Ord b) => (a, b) -> Relation a b -> Bool
member (a, b) = S.member b . image a

-- | /O(n)/ where @n@ is the number of distinct elements in the domain.
--
-- Compute the domain of the relation.
domain :: Relation a b -> S.Set a
domain = MS.keysSet . unRelation

-- | /O(size r)/. Compute the range of the relation.
range :: Ord b => Relation a b -> S.Set b
range = S.unions . MS.elems . unRelation

universe :: Ord a => Relation a a -> S.Set a
universe r = domain r <> range r

-- | /O(size r)/. @'reflexive' r@ iff for all @x `S.member` universe r@ it
-- holds that @(x,x) `'member'` r@.
reflexive :: Ord a => Relation a a -> Bool
reflexive r = and $ do
    (x, ys) <- MS.toList (unRelation r)
    return $ x `S.member` ys && ys `S.isSubsetOf` dom
  where
    dom = domain r

-- | /O(n * log n)/. @'symmetric' r@ iff for all @(x,y) `'member'` r@ it holds
-- that @(y,x) `'member'` r@.
symmetric :: Ord a => Relation a a -> Bool
symmetric r = all (\(x, y) -> (y, x) `member` r) (toList r)

-- | /O(n^2 * log n)/. @transitive r@ iff
-- @(x ,y) `member` r && @(y, z) `member` r@ implies @(x, z) `member` r@.
transitive :: Ord a => Relation a a -> Bool
transitive rel0 =
    size rel0 == size rel
  where
    rel = rel0 <> (rel0 `compose` rel0)



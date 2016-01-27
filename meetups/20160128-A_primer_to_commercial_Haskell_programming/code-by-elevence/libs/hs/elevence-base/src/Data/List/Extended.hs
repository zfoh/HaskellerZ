-- | An extended version of "Data.List" providing additional utilitites.
module Data.List.Extended
  ( module Data.List
  , snoc
  , unsnoc
  , sortedNub
  , unique
  , cyclic
  ) where


import           Control.Monad

import           Data.List
import qualified Data.Set as S

import           Prelude

import           Safe       (headNote)


-- | /O(n)/ Append an element at the end of the list.
snoc :: [a] -> a -> [a]
snoc xs x = xs ++ [x]

-- | /O(n)/ Extract the 'init' and 'last' of a list, returning 'Nothing' if
-- the list is empty.
--
-- > not (null xs) ==> unsnoc xs == Just (init xs, last x)
--
-- > maybe [] (uncurry snoc) (unsnoc xs) == xs
--
unsnoc :: [a] -> Maybe ([a], a)
unsnoc []       = Nothing
unsnoc (x0:xs0) = Just (go [] x0 xs0)
  where
    go prefix end []     = (reverse prefix, end)
    go prefix end (x:xs) = go (end:prefix) x xs

-- | /O(n * log(n))/. Sort and remove duplicates; i.e.,
-- @'sortedNub' = 'nub' .  'sort' = 'sort' . 'nub'@.
sortedNub :: Ord a => [a] -> [a]
sortedNub = map (headNote "sortednub") . group . sort

-- | /O(n * log(n))/. 'True' iff all elements in the list are different.
unique :: Ord a => [a] -> Bool
unique xs0 =
    and $ zipWith (/=) xs (tail xs)
  where
    xs = sort xs0

-- | /O(n^2)/. Is the relation cyclic.
--
-- TODO (SM): use an implementation with /O(n*log(n))/ complexity.
cyclic :: Ord a => [(a,a)] -> Bool
cyclic rel =
    maybe True (const False) $ foldM visitForest S.empty $ map fst rel
  where
    visitForest visited x
      | x `S.member` visited = return visited
      | otherwise            = findLoop S.empty visited x

    findLoop parents visited x
      | x `S.member` parents = mzero
      | x `S.member` visited = return visited
      | otherwise            =
          S.insert x <$> foldM (findLoop parents') visited next
      where
        next     = [ e' | (e,e') <- rel, e == x ]
        parents' = S.insert x parents

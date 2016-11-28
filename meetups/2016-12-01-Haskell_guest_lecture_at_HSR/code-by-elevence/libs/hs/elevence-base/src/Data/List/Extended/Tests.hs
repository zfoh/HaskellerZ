{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Tests for "Data.List.Extended"
module Data.List.Extended.Tests (tests) where


import           Data.List.Extended
import qualified Data.Relation.Binary.Ordered as RBO
import qualified Data.Set                     as S

import           Elevence.Prelude

import qualified Test.Tasty                      as Tasty
import qualified Test.Tasty.QuickCheck.Extended  as QC


------------------------------------------------------------------------------
-- Testing the 'unique' and 'cyclic' functions.
------------------------------------------------------------------------------

tests :: Tasty.TestTree
tests = Tasty.testGroup "Elevence.Prelude"
    [ QC.testProperty "snoc"      (defn_snoc      :: [Int] -> Bool)
    , QC.testProperty "unsnoc"    (defn_unsnoc    :: [Int] -> Bool)
    , QC.testProperty "sortedNub" (defn_sortedNub :: [Int] -> Bool)
    , QC.testProperty "unique"    (defn_unique    :: [Int] -> QC.Property)
    , QC.testProperty "cyclic"    (defn_cyclic    :: [(QC.SmallInt, QC.SmallInt)] -> QC.Property)
    ]

defn_snoc :: Eq a => [a] -> Bool
defn_snoc xs = xs == maybe [] (uncurry snoc) (unsnoc xs)

defn_unsnoc :: Eq a => [a] -> Bool
defn_unsnoc xs
    | null xs   = unsnoc xs == Nothing
    | otherwise = unsnoc xs == Just (init xs, last xs)

defn_sortedNub :: Ord a => [a] -> Bool
defn_sortedNub xs = nub (sort xs) == sortedNub xs

defn_unique :: Ord a => [a] -> QC.Property
defn_unique =
    QC.testPredicateDefinition unique unique_reference_impl
  where
    unique_reference_impl xs = length xs == S.size (S.fromList xs)

defn_cyclic :: Ord a => [(a, a)] -> QC.Property
defn_cyclic =
    QC.testPredicateDefinition cyclic cyclic_reference_impl
  where
    cyclic_reference_impl =
        any (uncurry (==)) . RBO.toList . RBO.transitiveClosure . RBO.fromList



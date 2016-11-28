{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoImplicitPrelude #-}
-- | Tests for "Data.Relation.Binary.Ordered".
module Data.Relation.Binary.Ordered.Tests (tests) where


import qualified Data.Relation.Binary.Ordered as RBO
import qualified Data.Set                     as S

import           Elevence.Prelude

import qualified Test.Tasty                      as Tasty
import qualified Test.Tasty.QuickCheck.Extended  as QC

type SmallIntRel = [(QC.SmallInt, QC.SmallInt)]

-- | TODO (SM): have a look at doctest and hspec to see how we could reduce
-- the redundancy in the documentation in the original module and the
-- testnames and implementations below.
tests :: Tasty.TestTree
tests = Tasty.testGroup "Data.Relation.Binary.Ordered"
    [ QC.testProperty "toList . fromList == nub . sort"
        (\xs -> nub (sort (xs :: SmallIntRel)) ==
                RBO.toList (RBO.fromList xs)
        )
    , QC.testProperty "domain . fromList == S.fromList . fmap snd"
        (\xs -> RBO.domain (RBO.fromList (xs :: SmallIntRel)) ==
                S.fromList (fmap fst xs)
        )
    , QC.testProperty "range . fromList == S.fromList . fmap snd"
        (\xs -> RBO.range (RBO.fromList (xs :: SmallIntRel)) ==
                S.fromList (fmap snd xs)
        )
    , QC.testProperty "size . fromList == length . nub"
        (\xs -> RBO.size (RBO.fromList (xs :: SmallIntRel)) ==
                length (nub xs)
        )
    , QC.testProperty "member . fromList == elem"
        (\xy xs -> RBO.member xy (RBO.fromList (xs :: SmallIntRel)) ==
                   elem xy xs
        )
    , QC.testProperty "image x . fromList == S.fromList . map snd . filter ((x ==) . fst)"
        (\x xs -> RBO.image x (RBO.fromList (xs :: SmallIntRel)) ==
                  S.fromList (map snd (filter ((x ==) . fst) xs))
        )
    , QC.testProperty "reflexive"
        (\xs ->
            let r = RBO.fromList (xs :: SmallIntRel)
                u = RBO.universe r
            in  RBO.reflexive r == all (\x -> (x, x) `RBO.member` r) (S.toList u)
        )
    , QC.testProperty "symmetric"
        (\xs ->
            -- NOTE (SM): currently equivalent to the definition of
            -- RBO.symmetric. This test will keep the implementation correct
            -- in case it is getting optimized.
            let r = RBO.fromList (xs :: SmallIntRel)
            in  all (\(x, y) -> (y, x) `RBO.member` r) xs == RBO.symmetric r
        )
    , QC.testProperty "transitive"
        (\xs ->
            let compose1 = [ (x, z) | (x, y1) <- xs, (y2, z) <- xs, y1 == y2 ]
                isTransitive = RBO.fromList (xs ++ compose1) == RBO.fromList xs
            in  isTransitive == RBO.transitive (RBO.fromList (xs :: SmallIntRel))
        )
    ]


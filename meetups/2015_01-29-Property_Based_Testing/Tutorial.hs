{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.SmartCheck
import GHC.Generics
import Data.Typeable
import Control.Monad
import Data.List

-- Simple properties on Lists
prop_RevUnit:: [Int] -> Bool
prop_RevUnit x =
    reverse [x] == [x]

prop_RevApp:: [Int] -> [Int] -> Bool
prop_RevApp xs ys =
    reverse (xs ++ ys) == reverse xs ++ reverse ys

prop_RevRev:: [Int] -> Bool
prop_RevRev xs =
    reverse (reverse xs) == xs

-- QuickSort
qsort []     = []
qsort (x:xs) = qsort lhs ++ [x] ++ qsort rhs
    where lhs  = [y | y <- xs, y < x]
          rhs  = [z | z <- xs, z > x]

isOrdered (x1:x2:xs) = x1 <= x2 && isOrdered (x2:xs)
isOrdered _          = True

prop_qsort_isOrdered :: [Int] -> Bool
prop_qsort_isOrdered = isOrdered . qsort

-- Model-based testing
prop_qsort_sort    :: [Int] -> Bool
prop_qsort_sort xs =  qsort xs == sort xs

--Conditional properties
alldiff (x:xs) = not (x `elem` xs) && alldiff xs
alldiff []     = True

prop_qsort_alldiff_sort :: [Int] -> Property
prop_qsort_alldiff_sort xs = 
   (alldiff xs) ==> qsort xs == sort xs

--Quantified properties
prop_insert_ordered :: Int -> [Int] -> Property 
prop_insert_ordered x xs = 
   isOrdered xs ==> isOrdered (insert x xs)

prop_insert_ordered2 x = forAll orderedList $ \xs -> isOrdered (insert x xs)
  where types = x::Int 

--Observing test case distribution
prop_insert_ordered3 :: Int -> [Int] -> Property 
prop_insert_ordered3 x xs = 
   --collect ((length xs `div` 10) * 10) $
   classify (isOrdered xs) "ord" $
   classify (not (isOrdered xs)) "not-ord" $
   not (isOrdered xs) || isOrdered (insert x xs)

--User-defined data generator
data Suit = Spades | Hearts | Diamonds | Clubs
  deriving (Show,Eq)

suit :: Gen Suit
suit = elements [Spades , Hearts , Diamonds , Clubs]

instance Arbitrary Suit where
  arbitrary = suit

data Rank = Numeric Integer | Jack | Queen | King | Ace
  deriving (Show,Eq)

rank :: Gen Rank
rank = oneof
    [ return Jack
    , return Queen
    , return King
    , return Ace
    , do r <- choose (2,10)
         return (Numeric r)
    ]

rank2 :: Gen Rank
rank2 = frequency
    [ (1, return Jack)
    , (1, return Queen)
    , (1, return King)
    , (1, return Ace)
    , (9, do r <- choose (2,10)
             return (Numeric r))
    ]

genList1 ::  (Arbitrary a) => Gen [a]
genList1 = liftM2 (:) arbitrary genList1

genList2 ::  (Arbitrary a) => Gen [a]
genList2 = oneof [ return []
                 , liftM2 (:) arbitrary genList2]

genList3 ::  (Arbitrary a) => Gen [a]
genList3 = frequency [ (1, return [])
                     , (4, liftM2 (:) arbitrary genList3) ]

genOrdList = fmap sort genList3

prop_insert :: Int -> Property 
prop_insert x = forAll genOrdList $ \xs -> isOrdered (insert x xs)

list :: Arbitrary a => Int -> Gen [a]
list 0 = return []
list n = liftM2 (:) arbitrary (list (n-1))

genList4 :: Arbitrary a => Gen [a]
genList4 = sized list

--Shrinking Binary Trees
data Tree a = Nil | Branch a (Tree a) (Tree a)
  deriving (Show,Eq,Typeable,Generic)

instance SubTypes (Tree Int)

tree:: Arbitrary a => Int -> Gen (Tree a)
tree 0 = return Nil
tree n = liftM3 Branch arbitrary (tree (n `div` 2)) (tree (n `div` 2))

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized tree 
    shrink Nil = []
    shrink (Branch x l r) =
        -- shrink Branch to Nil
        [Nil] ++
        -- shrink to subterms
        [l, r] ++
        -- recursively shrink subterms
        [Branch x' l' r' | (x', l', r') <- shrink (x, l, r)]

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

data Tree a = Nil | Branch a (Tree a) (Tree a)
  deriving (Show,Eq,Typeable,Generic)


tree:: Arbitrary a => Int -> Gen (Tree a)
tree 0 = return Nil
tree n = liftM3 Branch arbitrary (tree (n `div` 2)) (tree (n `div` 2))

instance Arbitrary a => Arbitrary (Tree a) where
    arbitrary = sized tree 

instance SubTypes (Tree Int)

sumtree:: (Num a, Arbitrary a) => Tree a -> a
sumtree Nil = 0
sumtree (Branch n b1 b2) = n + sumtree b1 + sumtree b2

prop t = (sumtree t) < 10

--quickCheck (prop :: Tree Int -> Bool)
--smartCheck scStdArgs (prop :: Tree Int -> Bool)

prop_RevApp_false:: ([Int],[Int]) -> Bool
prop_RevApp_false (xs,ys) =
    reverse (xs ++ ys) == reverse xs ++ reverse ys

main = smartCheck scStdArgs prop_RevApp_false

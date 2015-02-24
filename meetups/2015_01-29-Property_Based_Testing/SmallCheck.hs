{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

import Test.SmallCheck.Series
import Test.SmallCheck
import GHC.Generics
import Data.Typeable
import Control.Monad
import Data.List

isOrdered (x1:x2:xs) = x1 <= x2 && isOrdered (x2:xs)
isOrdered _          = True

prop_insert_ordered :: Monad m => Int -> [Int] -> Property m
prop_insert_ordered x xs = 
   isOrdered xs ==> isOrdered (insert x xs)

--smallCheck 7 prop_insert_ordered (too long)

-- data generation

data Tree a = Nil | Branch a (Tree a) (Tree a)
  deriving (Show,Eq,Typeable,Generic)

instance Serial m a => Serial m (Tree a)

sumtree:: Num a => Tree a -> a
sumtree Nil = 0
sumtree (Branch n b1 b2) = n + sumtree b1 + sumtree b2

prop:: Tree Int -> Bool
prop t = (sumtree t) < 10

--smallCheck 3 prop (too long for depth 4) 
prop_assoc op = \x y z ->
    (x `op` y) `op` z == x `op` (y `op` z)
    where typeInfo = op :: Bool -> Bool -> Bool

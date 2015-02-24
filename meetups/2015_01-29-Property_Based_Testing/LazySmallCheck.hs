{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Test.LazySmallCheck
import Data.List

isOrdered (x1:x2:xs) = x1 <= x2 && isOrdered (x2:xs)
isOrdered _          = True

prop_insert_ordered :: Int -> [Int] -> Bool 
prop_insert_ordered x xs = 
   isOrdered xs ==> isOrdered (insert x xs)

{-# LANGUAGE BangPatterns #-}
module ListExamples where

data List a = Nil | One a (List a)


{-# INLINABLE sumList #-}
sumList :: Num a => List a -> a
sumList =
    go 0
  where
    -- go :: Int -> List Int -> Int
    go !accum Nil        = accum
    go !accum (One x xs) = go (accum + x) xs


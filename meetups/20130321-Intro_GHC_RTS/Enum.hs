{-# LANGUAGE BangPatterns #-}
module Test where

data List a = Nil | Cons a (List a)

-- mapList :: (a -> b) -> List a -> List b
mapList f Nil         = ones
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

ones = Cons 1 ones

{-
twos = map' (+1) ones

-}

{-
sumNaive :: [Int] -> Int
sumNaive [] = 0
sumNaive (x:xs) = x + sumNaive xs


sumAccum :: [Int] -> Int
sumAccum =
    go 0
  where
    go acc []     = acc
    go acc (x:xs) = go (x + acc) xs

data Tree a = Leaf | Node Int (Tree a) (Tree a)

treeSumAccum :: Tree Int -> Int
treeSumAccum =
    go 0
  where
    go acc Leaf         = acc
    go acc (Node x l r) = go (x + go (acc + x) l) r

avgAccum :: [Double] -> Maybe Double
avgAccum =
    go 0 0
  where
    go cnt sum []
      | cnt == 0  = Nothing
      | otherwise = Just (sum / cnt)
    go cnt sum (x:xs) = go (cnt + 1) (sum + x) xs


sumAccumStrict :: [Int] -> Int
sumAccumStrict =
    go 0
  where
    go !acc []     = acc
    go !acc (x:xs) = go (x + acc) xs

avgAccumStrict :: [Double] -> Maybe Double
avgAccumStrict []     = Nothing
avgAccumStrict (x:xs) = Just (go 1 x xs)
  where
    go cnt sum []     = sum / cnt
    go cnt sum (x:xs) = go (cnt + 1) (sum + x) xs

-}

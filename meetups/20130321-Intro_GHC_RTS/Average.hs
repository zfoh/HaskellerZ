{-# LANGUAGE BangPatterns #-}
module Test where

avgAccum :: [Double] -> Maybe Double
avgAccum =
    go 0 0
  where
    go cnt sum []
      | cnt == 0  = Nothing
      | otherwise = Just (sum / cnt)
    go cnt sum (x:xs) = go (cnt + 1) (sum + x) xs

avgAccumBang :: [Double] -> Maybe Double
avgAccumBang =
    go 0 0
  where
    go !cnt !sum []
      | cnt == 0  = Nothing
      | otherwise = Just (sum / cnt)
    go cnt sum (x:xs) = go (cnt + 1) (sum + x) xs

avgAccumStrict :: [Double] -> Maybe Double
avgAccumStrict []     = Nothing
avgAccumStrict (x:xs) = Just (go 1 x xs)
  where
    go cnt sum []     = sum / cnt
    go cnt sum (x:xs) = go (cnt + 1) (sum + x) xs

avgAccumStrictBang :: [Double] -> Maybe Double
avgAccumStrictBang []     = Nothing
avgAccumStrictBang (x:xs) = Just (go 1 x xs)
  where
    go !cnt !sum []     = sum / cnt
    go !cnt !sum (x:xs) = go (cnt + 1) (sum + x) xs


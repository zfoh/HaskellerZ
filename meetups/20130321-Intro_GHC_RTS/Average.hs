{-# LANGUAGE BangPatterns #-}
module Test where

import Data.Maybe

avgAccum :: [Double] -> Maybe Double
avgAccum []     = Nothing
avgAccum (x:xs) = Just (go 1 x xs)
  where
    go :: Double -> Double -> [Double] -> Double
    go cnt sum []     = sum / cnt
    go cnt sum (x:xs) = go (cnt + 1) (sum + x) xs


forcedAvg :: [Double] -> Double
forcedAvg = fromJust . avgAccum

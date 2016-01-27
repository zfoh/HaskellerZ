{-# LANGUAGE NoImplicitPrelude #-}
module Data.Map.Strict.Extended
  ( module Data.Map.Strict
  , subMap
  ) where

import           Data.Map.Strict
import qualified Data.Set as S

import           Elevence.Prelude


-- | Reduce the domain of the map to the given set.
subMap :: Ord k => S.Set k -> Map k a -> Map k a
subMap s m = intersection m (fromSet (const ()) s)

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}

import Test.QuickCheck ( Arbitrary(arbitrary), quickCheck, sized )
import Data.Functor () 
import Test.Feat ( Enumerable, deriveEnumerable, uniform )
import Data.Typeable ( Typeable )
import Text.Show.Functions ()

data Tree a = Fork (Tree a) (Tree a) | Leaf a
  deriving (Show, Eq, Typeable)
  
deriveEnumerable ''Tree

instance (Enumerable a, Arbitrary a) => Arbitrary (Tree a) where
  arbitrary = sized uniform

instance Functor Tree where
  fmap f (Fork t u) = Fork (fmap f t) (fmap f u)
  fmap f (Leaf x) = Leaf (f x)

law1 :: Tree Int -> Bool
law1 t = fmap id t == t

law2 :: (Int -> Int) -> (Int -> Int) -> Tree Int -> Bool
law2 f g t = fmap (f . g) t == fmap f (fmap g t)

main = do
  quickCheck law1
  quickCheck law2

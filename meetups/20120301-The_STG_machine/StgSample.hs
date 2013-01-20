-- | Examples from the HaskellerZ meeting on the STG machine described in
-- Simon Peyton Jonas '92 paper.
--
-- Use the Makefile in the same directory for producing the corresponding
-- 'dump.txt' file.
--
-- Date: 2012/03/01
module Test where


map' :: (a -> b) -> [a] -> [b]
map' f []     = []
map' f (x:xs) = f x : map' f xs

-- ones = (1 :: Int) : ones

-- first []     = error "blah"
-- first (x:xs) = x



-- sum2 (x:y:_) = x + y
-- sum2 _       = error "blah"

-- test = sum2 ones

{-
import Control.Applicative
import Data.Traversable

data List a = Nil | Cons a (List a)

mapSub :: (a -> b) -> [a] -> List b
mapSub f =
    go
  where
    go []     = Nil
    go (x:xs) = Cons (f x) (go xs)

traverseSub :: Applicative f => (a -> f b) -> [a] -> f (List b)
traverseSub f =
    go
  where
    go []     = pure Nil
    go (x:xs) = Cons <$> f x <*> go xs
-}

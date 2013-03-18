{-# LANGUAGE BangPatterns #-}
module ListExamples where

{-
data ListInt = NilI | OneI {-# UNPACK #-} !Int ListInt

sumN :: Int -> Int
sumN 0 = 0
sumN n = n + sumN (n - 1)

{-
mapListInt :: (Int -> Int) -> ListInt -> ListInt
mapListInt f NilI        = NilI
mapListInt f (OneI x xs) = OneI (f x) (mapListInt f xs)
{-# INLINE mapListInt #-}
-}

mapListInt :: (Int -> Int) -> ListInt -> ListInt
mapListInt f =
    go
  where
    go NilI        = NilI
    go (OneI x xs) = OneI (f x) (go xs)
{-# INLINE mapListInt #-}

sumListInt :: ListInt -> Int
sumListInt NilI        = 0
sumListInt (OneI x xs) = x + sumListInt xs

ex1 :: ListInt
ex1 = OneI 1 (OneI 2 (OneI 3 NilI))

foo :: Int -> Int
foo x = x + 999

test :: Int -> Int
test x = sumListInt (mapListInt foo (OneI x ex1))
-}

{- Non-strict version -}


data List a = Nil | One a (List a)

-- mapList :: (a -> b) -> List a -> List b
-- mapList f Nil         = Nil
-- mapList f (One x xs) = One (f x) (mapList f xs)

{-
sumList :: List Int -> Int
sumList Nil        = 0
sumList (One x xs) = x + sumList xs

ex1 :: List Int
ex1 = One 1 (One 2 (One 3 Nil))

test = sumList ex1
-}

mapList :: (a -> b) -> List a -> List b
mapList f Nil        = Nil
mapList f (One x xs) = One (f x) (mapList f xs)

foo :: Int -> Int
foo x = x + 0xDeadBeef

test :: List Int
test = mapList foo (One 1 (One 2 (One 3 Nil)))

test' :: Int
test' = case test of One x _ -> x
{-
foo :: Int -> Int
foo x = x + 999

test :: Int
test = sumList (mapList foo ex1)
-}












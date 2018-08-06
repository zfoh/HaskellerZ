module StackLang

import Data.Vect

-- a DSL for a simple stack language
data StackLang : Type -> Vect h1 Type -> Vect h2 Type -> Type where
  Push : a -> StackLang () xs (a :: xs)
  Pop : StackLang a (a :: xs) xs

  Add : Num a => a -> a -> StackLang a v v
  Mul : Num a => a -> a -> StackLang a v v

  Pure : a -> StackLang a v v
  (>>=) : StackLang a v1 v2 ->
          (a -> StackLang b v2 v3) -> StackLang b v1 v3

-- dup : StackLang () (ty :: s) (ty :: ty :: s)
-- dup = do
--   x <- Pop
--   Push x
--   Push x

-- swap : StackLang () (t1 :: t2 :: s) (t2 :: t1 :: s)
-- swap = do
--   x <- Pop
--   y <- Pop
--   Push x
--   Push y

-- add : Num ty => StackLang () (ty :: ty :: s) (ty :: s)
-- add = do
--   x <- Pop
--   y <- Pop
--   z <- Add x y
--   Push z

-- mul : Num ty => StackLang () (ty :: ty :: s) (ty :: s)
-- mul = do
--   x <- Pop
--   y <- Pop
--   z <- Mul x y
--   Push z

-- square : Num ty => StackLang () (ty :: s) (ty :: s)
-- square = do dup; mul

-- squareSum : Num ty => StackLang () (ty :: ty :: s) (ty :: s)
-- squareSum = do square; swap; square; add

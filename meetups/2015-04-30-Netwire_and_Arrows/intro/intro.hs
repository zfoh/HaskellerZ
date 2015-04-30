{-# LANGUAGE Arrows #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Wire
import FRP.Netwire
import Prelude hiding ((.), id)

import NetwireTools


--------------------------------------------------------------------------------

w1 :: SimpleWire a Bool
w1 = pure True

-- `SimpleWire` is a convenience type, that fills out the first three
-- type parameters of `Wire` with sensible values.

testW1 :: IO ()
testW1 = testSimpleWire w1

-- `testSimpleWire` is analogous to the `testWire clockSession_` from
-- Netwire's tutorial, but doesn't hog the CPU.


w2 :: SimpleWire a Int
w2 = (+) <$> pure 5 <*> pure 10




w3 :: SimpleWire a Int
w3 = 42




--------------------------------------------------------------------------------

-- Example that actually depends on time?

time' :: SimpleWire a NominalDiffTime
time' = time

-- :i time
--
-- The meaning of the first type parameter...


w4 :: SimpleWire a Double
w4 = 10 + timeF / 2


-- timeF = realToFrac <$> time

w5 :: SimpleWire a Double
w5 = (sin $ timeF / 4) ^ 2



timeScaled :: Fractional b => b -> SimpleWire a b
timeScaled scale = undefined


--------------------------------------------------------------------------------

-- Example that actually depends on its input?

w6 :: SimpleWire a Double
w6 = integral 0 . 10

-- :i integral




--------------------------------------------------------------------------------

-- What's about the `Alternative` instance?
--
-- Wires can _inhibit_.


w7 :: SimpleWire a Int
w7 = for 2 . pure 10

-- :i for
--
-- The meaning of the second type parameter...
--
-- And, then also for the third...


w8 :: SimpleWire a Int
w8 = for 2 . 10  <|>  after 3 . 42  <|>  5


--------------------------------------------------------------------------------

-- Switching to a different _behavior_...

w9 :: SimpleWire a Double
w9 = for 2 . timeF  -->  42

-- Switching "frees up" the original wire (so stuff can be garbage
-- collected).

-- The "time" is local in the wires: only starts when the wire is
-- first activated:

w10 :: SimpleWire a NominalDiffTime
w10 = for 2 . time  -->  time

-- What happens if we switch --> to <|> ?
-- Are the inactive wires run?
-- How can we test this?



-- Switching can be recursive.

counter :: (Monad m, Monoid e, HasTime t s) => t -> Wire s e m a Int
counter holdTime = loop 0
  where
    loop k = for holdTime . pure k --> loop (k+1)






--------------------------------------------------------------------------------

-- Events.
--
-- See slides. :)


--------------------------------------------------------------------------------

netwireIsCool :: SimpleWire a String
netwireIsCool =
    for 2 . "Once upon a time..." -->
    for 3 . "... games were completely imperative..." -->
    for 2 . "... but then..." -->
    for 10 . ("Netwire 5! " <> anim) -->
    netwireIsCool

  where
    anim =
        holdFor 0.5 . periodic 1 . "Hoo..." <|>
        "...ray!"


main :: IO ()
main = testSimpleWire netwireIsCool


--------------------------------------------------------------------------------

-- Can we define an if-then-else combinator?

ifteWire :: Wire s e m a Bool
         -> Wire s e m a b
         -> Wire s e m a b
         -> Wire s e m a b
ifteWire condW thenW elseW = undefined








--------------------------------------------------------------------------------

ifteA :: Applicative f => f Bool -> f a -> f a -> f a
ifteA condA thenA elseA
  = (\c a b -> if c then a else b) <$> condA <*> thenA <*> elseA


if1 :: SimpleWire a Int
if1 = ifteA cond one two
  where
    cond = for 3 . pure True --> pure False
    one = counter 1
    two = counter 0.5






--------------------------------------------------------------------------------

ifte :: ArrowChoice arr => arr a Bool -> arr a b -> arr a b -> arr a b
ifte condA thenA elseA = proc a -> do
  p <- condA -< a
  if p
    then thenA  -< a
    else elseA -< a

if2 :: SimpleWire a Int
if2 = ifte cond one two
  where
    cond = for 3 . pure True --> pure False
    one = counter 1
    two = counter 0.5

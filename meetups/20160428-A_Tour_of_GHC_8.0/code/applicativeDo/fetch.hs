{-# LANGUAGE InstanceSigs #-}

import Control.Monad (ap)

data Fetch a = Done a | Blocked (Fetch a) deriving (Show)

instance Functor Fetch where
  fmap f (Done a) = Done (f a)
  fmap f (Blocked c) = Blocked (fmap f c)

instance Applicative Fetch where
  pure a = Done a

  (<*>) :: Fetch (a -> b) -> Fetch a -> Fetch b
  Done    f <*> Done    a = Done    (f          a)
  Done    f <*> Blocked a = Blocked (f <$>      a)
  Blocked f <*> Done    a = Blocked (f <*> Done a)
  Blocked f <*> Blocked a = Blocked (f <*>      a)

instance Monad Fetch where
  return = pure

  (>>=) :: Fetch a -> (a -> Fetch b) -> Fetch b
  Done a    >>= f = f a
  Blocked a >>= f = Blocked (a >>= f)

exampleApplicative :: Fetch Int
exampleApplicative = Blocked (Done (+1)) <*> Blocked (Done 1)

exampleMonadic :: Fetch Int
exampleMonadic = Blocked (Done (+1)) `ap` Blocked (Done 1)

main :: IO ()
main = do
  print $ exampleApplicative
  print $ exampleMonadic

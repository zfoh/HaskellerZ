-- This module was not used during the talk.

module Either where

import Prelude hiding (Either, Right, Left, Monad, (>>=), return)
import Monad

data Either a b
    = Left a
    | Right b
  deriving (Read, Show, Eq, Ord)

data User
    = User
    { userName :: Either String String
    }
  deriving (Show, Read, Eq, Ord)

user :: Int -> Either String User
user 1 = Right $ User (Right "SPJ")
user 2 = Right $ User (Right "ekmett")
user 3 = Right $ User (Left "Privacy!")
user 4 = Right $ User (Right "")
user 5 = Left "No such user."
user _ = Left "Bad ID."

instance Monad (Either a) where
    -- (>>=) :: Either a b -> (b -> Either a c) -> Either a c
    x >>= f = case x of
                Left err -> Left err
                Right b  -> f b
    return = Right

userNameForID :: Int -> Either String String
userNameForID = user >=> userName

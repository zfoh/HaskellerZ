module Maybe where

import Prelude hiding (Maybe, Just, Nothing, Monad, (>>=), (>=>), return)

data Maybe a
    = Just a
    | Nothing
  deriving (Eq, Ord, Show, Read)

data User = User { userName :: Maybe String }
  deriving (Eq, Ord, Read, Show)

-- userName :: User -> Maybe String

user :: Int -> Maybe User
user 1 = Just $ User (Just "Jannis")
user 2 = Just $ User (Just "SPJ")
user 3 = Just $ User (Just "Comic Sans")
user 4 = Just $ User Nothing
user 5 = Just $ User (Just "")
user _ = Nothing

firstCharForID :: Int -> Maybe Char
firstCharForID = user >=> userName >=> firstChar

firstChar :: String -> Maybe Char
firstChar []    = Nothing
firstChar (c:_) = Just c

-- "Fish", "Kleisli Composition"
(>=>) :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
f >=> g = \a -> f a >>= g

class Monad m where
    -- "Bind", m a === "Action", (a -> m b) === "Computation"
    (>>=) :: m a -> (a -> m b) -> m b
    return :: a -> m a

instance Monad Maybe where
    m >>= f =
      case m of
        Nothing -> Nothing
        Just x  -> f x

    return = Just

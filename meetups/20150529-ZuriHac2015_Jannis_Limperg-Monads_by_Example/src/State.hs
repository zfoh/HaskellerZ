module State where

import Prelude hiding (Monad, (>>=), (>>), return)
import Monad

newtype State s a = State (s -> (a, s))

instance Monad (State s) where
    -- (>>=) :: (s -> (a, s)) -> (a -> s -> (b, s)) -> (s -> (b, s))
    (State st) >>= f = State $ \initialState ->
      case st initialState of
        (a, newState) -> case f a of
                           (State st') -> st' newState

    return x = State $ \state -> (x, state)

get :: State s s
get = State $ \state -> (state, state)

put :: s -> State s ()
put newState = State $ \_ -> ((), newState)

modify :: (s -> s) -> State s ()
modify f = State $ \state -> ((), f state)

randomElement :: [a] -> State RNG (Maybe a)
randomElement = undefined

test = randomElement [1 .. 5] >>= \n -> randomElement [1 .. n]
test2 = mapM randomElement [[1..5], [3], []]

evalState :: s -> State s a -> a
evalState initialState (State st) -> fst $ st initialState

-- Required for the MaybeC example below.
{-# LANGUAGE Rank2Types #-}

-- | HaskellerZ sketch-book from Juli 11th, 2011
module HaskellerZ where

import qualified Data.Map as M
import Control.Monad.State
import Control.Monad.Identity()

-- Stacking a State monad on top of Maybe
-----------------------------------------

type SymTab = M.Map String String 

type CompileState = SymTab 

type CompileM a = StateT CompileState Maybe a

table0 :: SymTab
table0 = undefined

runCompileM :: SymTab -> CompileM a -> Maybe (a, SymTab)
runCompileM symbols0 m = runStateT m symbols0

returnCompileM :: a -> CompileM a
returnCompileM x = StateT $ \s -> Just (x, s)

bindCompileM :: CompileM a -> (a -> CompileM b) -> CompileM b
bindCompileM m f = StateT $ \s -> 
    case runStateT m s of
        Just (x, s') -> runStateT (f x) s'
        Nothing      -> Nothing

-- A continuation passing implementation of Maybe
-------------------------------------------------

-- cf. Edward Kmett's series on Free Monads for Less
--
--   http://comonad.com/reader/2011/free-monads-for-less/
--
-- There you can find the math behind types such as the one below.
--

data MaybeC a = MaybeC { unMaybeC :: forall r. r -> (a -> r) -> r }

-- | Converting continuation passing style to tagged values.
runMaybeC :: MaybeC a -> Maybe a
runMaybeC m = unMaybeC m Nothing Just

-- | Converting tagged values to continuation passing style
liftMaybe :: Maybe a -> MaybeC a
liftMaybe (Just x) = MaybeC $ \_        fJust -> fJust x
liftMaybe Nothing  = MaybeC $ \fNothing _     -> fNothing

returnMaybeC :: a -> MaybeC a
returnMaybeC x = MaybeC $ \_ fJust -> fJust x

fmapMaybeC :: (a -> b) -> MaybeC a -> MaybeC b
fmapMaybeC f m = MaybeC $ \fNothing fJust -> unMaybeC m fNothing (fJust . f)

bindMaybeC :: MaybeC a -> (a -> MaybeC b) -> MaybeC b
bindMaybeC = error "this could provide a nice exercise"


{-# LANGUAGE TemplateHaskell #-}
module Lib
    ( runTests
    ) where

import           Control.Monad

import qualified Data.Map.Strict as MS
import           Data.Maybe

import           Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range

------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

type VarId = String
type ConId = String

data Term = Var VarId | App ConId [Term]
  deriving (Eq, Show)

type Subst = MS.Map VarId Term


------------------------------------------------------------------------------
-- Substitution
------------------------------------------------------------------------------

empty :: Subst
empty = MS.empty

apply :: Term -> Subst -> Term
apply t s = case t of
  Var v    -> fromMaybe (Var v) $ MS.lookup v s
  App c ts -> App c (map (`apply` s) ts)


------------------------------------------------------------------------------
-- Unification
------------------------------------------------------------------------------

unify :: Term -> Term -> Maybe Subst
unify t1 t2 = do
  guard (t1 == t2)
  pure empty


------------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------------

runTests :: IO ()
runTests = do
  recheck (Size 76) (Seed 18188614901930827948 9667953824941721235) prop_unifiable_unifies
  recheck (Size 11) (Seed 15564363626265308103 4172507101916259409) prop_unifiable_unifies
  -- void $ checkParallel $$(discover)


-- Generators
-------------

-- | Generate terms from variables "a" and "b"; and constructros "Int",
-- "Maybe" and "Pair"
termG :: Gen Term
termG =
  Gen.recursive Gen.choice [
      -- non-recursive generators
      Var <$> Gen.element ["a", "b"]  -- assume few variables suffice
    , pure $ App "Int" []
    ] [
      -- recursive generators
      Gen.subterm  termG (\x -> App "Maybe" [x])
    , Gen.subterm2 termG termG (\x y -> App "Pair" [x, y])
    ]




substG :: Gen Subst
substG = do
    ta <- termG
    tb <- termG
    let trivial (v, t) = Var v == t
    pure $ MS.fromList $ filter (not . trivial) $ [("a", ta), ("b", tb)]


-- Properties
-------------

--  ∀ t1 t2 σ. t₁σ = t₂σ ⇒ (∃ σ'. unify t1 t2 = Just σ' ∧ t₁σ' = t₂σ')


prop_unifiable_unifies = withDiscards 10000 $ property $ do
    t1 <- forAll termG
    t2 <- forAll termG
    s  <- forAll substG
    unless (apply t1 s == apply t2 s) discard
    case unify t1 t2 of
      Nothing -> failure
      Just s' -> apply t1 s' === apply t2 s'






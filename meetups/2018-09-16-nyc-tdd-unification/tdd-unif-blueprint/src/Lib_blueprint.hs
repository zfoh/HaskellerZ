{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -Wall #-}
module LibNyc where


import           Control.Monad

import qualified Data.Set        as S
import qualified Data.Map.Strict as MS

import           Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen

------------------------------------------------------------------------------
-- Terms, Substitutions, and Unification
------------------------------------------------------------------------------

type VarId = String
type ConId = String

data Term = Var VarId | App ConId [Term]
  deriving (Eq, Show)

type Subst = MS.Map VarId Term



------------------------------------------------------------------------------
-- Test Execution
------------------------------------------------------------------------------

runTests :: IO ()
runTests = do
    mapM_ unit_test_apply $ applyTestCases
    mapM_ unit_test_unifies $ take 2 unifyTestCases
    void $ checkParallel $$(discover)


------------------------------------------------------------------------------
-- Unit Test Specification
------------------------------------------------------------------------------

-- smart constructors
---------------------

vaT, vbT, intT :: Term
vaT       = Var "a"
vbT       = Var "b"
intT      = App "Int" []

maybeT :: Term -> Term
maybeT x  = App "Maybe" [x]

pairT :: Term -> Term -> Term
pairT x y = App "Pair" [x, y]

mkSubst :: [(VarId, Term)] -> Subst
mkSubst = MS.fromList


-- test data
------------

applyTestCases :: [(Term, Subst, Term)]
applyTestCases =
  [ (intT,          mkSubst [("b", vaT)            ], intT)
  , (pairT vaT vbT, mkSubst [("b", intT)           ], pairT vaT intT)
  , (pairT vaT vbT, mkSubst [("b", vaT), ("a", vbT)], pairT vbT vaT)
  ]


unifyTestCases :: [(Term, Term, Maybe Subst)]
unifyTestCases =
  [ (vaT,               intT,            solvable [("a", intT)])
  , (intT,              maybeT vaT,      unsolvable)
  , (vaT,               maybeT vaT,      unsolvable)
  , (pairT vaT vbT,     maybeT vaT,      unsolvable)
  , (App "Pair" [intT], pairT intT intT, unsolvable)
  , (pairT vaT vaT,     pairT vbT intT,  solvable [("a", intT), ("b", intT)])
  , (pairT vaT vbT,     pairT vaT intT,  solvable [("b", intT)             ])
  , (pairT vaT vbT,     pairT vbT intT,  solvable [("a", intT), ("b", intT)])
  , (pairT vaT vbT,     pairT intT vaT,  solvable [("a", intT), ("b", intT)])
  , (pairT vaT vbT,     pairT vbT vaT,   solvable [("a", vbT)              ])
  ]
  where
    solvable   = Just . mkSubst
    unsolvable = Nothing

-- test case executors
----------------------

unit_test_apply :: (Term, Subst, Term) -> IO ()
unit_test_apply tc@(t, s, result) = checkUnitTest $ do
    annotateShow tc
    apply s t === result

unit_test_unifies :: (Term, Term, Maybe Subst) -> IO ()
unit_test_unifies tc@(t1, t2, result) = checkUnitTest $ do
    annotateShow tc

    -- check test case validity (as far as possible)
    case result of
      Just s  -> apply s t1 === apply s t2
      Nothing -> pure () -- cannot test

    unify t1 t2 === result

-- | Helper function to run a unit-test.
checkUnitTest :: PropertyT IO () -> IO ()
checkUnitTest = void . check . withTests 1 . withDiscards 1 . property


------------------------------------------------------------------------------
-- Property Test Specifications
------------------------------------------------------------------------------

-- ∀ t₁ t₂ σ. t₁σ = t₂σ
--   ⇒ (∃ σ’. unify t₁ t₂ = Just σ’ ∧ t₁σ’ = t₂σ’)

prop_unifiable_unifies :: Property
prop_unifiable_unifies = withDiscards 10000 $ withTests 100 $ property $ do
    t1 <- forAll termG
    t2 <- forAll termG
    s  <- forAll substG
    unless (apply s t1 == apply s t2) discard
    case unify t1 t2 of
      Nothing -> failure
      Just s' -> apply s' t1 === apply s' t2

varG :: Gen VarId
varG = Gen.element ["a", "b"]

termG :: Gen Term
termG = Gen.recursive Gen.choice [
      -- non-recursive generators
      Var <$> varG
    , pure (App "Int" [])
    ] [
      -- recursive generators
      Gen.subterm  termG maybeT
    , Gen.subterm2 termG termG pairT
    ]

substG :: Gen Subst
substG = do
   ta <- termG
   tb <- termG
   pure $ MS.fromList [ (v, t) | (v, t) <- [("a",ta), ("b",tb)] , Var v /= t ]




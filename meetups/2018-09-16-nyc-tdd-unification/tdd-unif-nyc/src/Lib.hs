{-# LANGUAGE TemplateHaskell #-}
-- {-# OPTIONS_GHC -Wall #-}
module Lib where


import           Control.Monad

import qualified Data.Set        as S
import qualified Data.Map.Strict as MS

import           Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen

------------------------------------------------------------------------------
-- Terms, Substition, Unification
------------------------------------------------------------------------------

type VarId = String
type ConId = String

data Term =
    Var VarId
  | App ConId [Term]
  deriving (Eq, Show)

type Subst = MS.Map VarId Term

emptySubst :: Subst
emptySubst = MS.empty

singletonSubst :: VarId -> Term -> Subst
singletonSubst = MS.singleton

apply :: Subst -> Term -> Term
apply s t = case t of
  Var v    -> MS.findWithDefault t v s
  App c ts -> App c (map (apply s) ts)


-- compose :: Subst -> Subst -> Subst

unify :: Term -> Term -> Maybe Subst
unify t1 t2 = solve emptySubst [(t1, t2)]

solve :: Subst -> [(Term, Term)] -> Maybe Subst      
solve s [] = Just s
solve s ((t1,t2):eqs) = case (t1', t2') of
    (Var v1, _)              -> elim v1 t2' 
    (_     , Var v2)         -> elim v2 t1'
    (App c1 ts1, App c2 ts2) -> do
      guard (c1 == c2)
      guard (length ts1 == length ts2)
      solve s (eqs ++ zip ts1 ts2)
  where
    t1' = apply s t1
    t2' = apply s t2

    elim v t
      | Var v == t              = solve s eqs
      | v `S.member` freeVars t = Nothing
      | otherwise               = solve (MS.insert v t s') eqs
      where
        s' = (MS.map (apply (singletonSubst v t)) s)
        


freeVars :: Term -> S.Set VarId
freeVars t = case t of
  Var v    -> S.singleton v
  App _ ts -> foldMap freeVars ts
  

------------------------------------------------------------------------------
-- Test Execution
------------------------------------------------------------------------------

runTests :: IO ()
runTests = do
    -- mapM_ unit_test_apply $ applyTestCases
    mapM_ unit_test_unifies $ unifyTestCases
    void $ checkParallel $$(discover)
    recheck (Size 1) (Seed 5122919457132373076 8099718030324741571) prop_unifiable_unifies
    recheck (Size 5) (Seed 2673809100440509064 10288130836413145155) prop_unifiable_unifies


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

prop_apply_identity = property $ do
   t <- forAll termG
   apply emptySubst t === t

varG :: Gen VarId
varG = Gen.element ["a", "b", "c"]

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
   tc <- termG
   pure $ MS.fromList
     [ (v, t)
     | (v, t) <- [("a",ta), ("b",tb), ("c", tc)]
     , Var v /= t
     ]




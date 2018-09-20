{-# LANGUAGE TemplateHaskell #-}

module Lib where


import           Control.Monad

import qualified Data.Set        as S
import qualified Data.Map.Strict as MS

import           Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range

------------------------------------------------------------------------------
-- Terms
------------------------------------------------------------------------------

type VarId = String
type ConId = String

data Term = Var VarId | App ConId [Term]
  deriving (Eq, Show)


pair x y = App "Pair" [x, y]
int      = App "Int" []

------------------------------------------------------------------------------
-- Substitutions
------------------------------------------------------------------------------
type Subst = MS.Map VarId Term

empty :: Subst
empty = MS.empty

apply :: Term -> Subst -> Term
apply t s = case t of
  Var v    -> MS.findWithDefault (Var v) v s
  App c ts -> App c (map (`apply` s) ts)

unify :: Term -> Term -> Maybe Subst
unify t1 t2 = solve empty [(t1, t2)]

solve :: Subst -> [(Term, Term)] -> Maybe Subst
solve s []            = Just s
solve s ((t1,t2):eqs)
  | t1' == t2' = solve s eqs
  | otherwise  = case (t1', t2') of
      (Var v1, _     )          -> elim v1 t2'
      (_     , Var v2)          -> elim v2 t1'
      (App c1 ts1 , App c2 ts2) -> do
        guard (c1 == c2)
        guard (length ts1 == length ts2)
        solve s (zip ts1 ts2 ++ eqs)
  where
    t1' = apply t1 s
    t2' = apply t2 s

    elim v t
      | v `S.member` vars t = Nothing
      | otherwise           = solve (compose1 v t s) eqs

compose1 :: VarId -> Term -> Subst -> Subst
compose1 v t s = MS.insert v t $ MS.map (`apply` MS.singleton v t) s


vars :: Term -> S.Set VarId
vars t = case t of
  Var v    -> S.singleton v
  App c ts -> foldMap vars ts


------------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------------

runTests :: IO ()
runTests = void $
  checkParallel $$(discover)


------------------------------------------------------------------------------
-- Generators
------------------------------------------------------------------------------

termG :: Gen Term
termG = Gen.recursive Gen.choice [
      -- non-recursive generators
      Var <$> Gen.element ["a", "b", "c"]
    , pure (App "Int" [])
    ] [
      -- recursive generators
      Gen.subterm  termG (\t -> App "Maybe" [t])
    , Gen.subterm2 termG termG (\t1 t2 -> App "Pair" [t1, t2])
    ]



substG :: Gen Subst
substG = do
   ta <- termG
   tb <- termG
   tc <- termG
   let s = MS.fromList
         [ (v, t)
         | (v, t) <- [("a",ta), ("b",tb), ("c",tc)]
         , Var v /= t
         ]
   pure s


------------------------------------------------------------------------------
-- Properties
------------------------------------------------------------------------------

prop_apply_unitTest1 :: Property
prop_apply_unitTest1 = withTests 1 $ property $ do
    apply t s === r
  where
    t   = Var "a"
    s   = MS.fromList [("a", int)]
    r   = int
    int = App "Int" []

-- ∀ t₁ t₂ σ. t₁σ = t₂σ
--   ⇒ (∃ σ’. unify t₁ t₂ = Just σ’ ∧ t₁σ’ = t₂σ’)


prop_unifiable_unifies :: Property
prop_unifiable_unifies = withDiscards 10000 $ withTests 100 $ property $ do
    t1 <- forAll termG
    t2 <- forAll termG
    s  <- forAll substG
    unless (apply t1 s == apply t2 s) discard
    case unify t1 t2 of
      Nothing -> failure
      Just s' -> apply t1 s' === apply t2 s'

prop_unifiable_unitTest1 :: Property
prop_unifiable_unitTest1 = withTests 1 $ property $ do
    case unify t1 t2 of
      Nothing -> pure ()
      Just s' -> do
        annotateShow s'
        failure
  where
    t1 = pair (Var "a") (Var "a")
    t2 = Var "a"

prop_unifiable_unitTest2 :: Property
prop_unifiable_unitTest2 = withTests 1 $ property $ do
    case unify t1 t2 of
      Nothing -> failure
      Just s' -> do
        annotateShow s'
        apply t1 s' === apply t2 s'
  where
    t1 = pair (Var "a") (Var "b")
    t2 = pair (Var "b") int


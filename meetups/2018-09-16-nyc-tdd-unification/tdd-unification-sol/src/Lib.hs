{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where


import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Except

import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Map.Strict as MS
import qualified Data.Set        as S

import           Hedgehog hiding (Var)
import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range

------------------------------------------------------------------------------
-- Test runner
------------------------------------------------------------------------------



------------------------------------------------------------------------------
-- Terms
------------------------------------------------------------------------------

-- | Identifiers for constructors.
type ConId = T.Text

-- | Identifiers for variables.
type VarId = T.Text

-- | Terms are built from variables and applications of constructors
-- to types.
data Term
  = Var    VarId
  | App ConId [Term]
  deriving (Eq, Ord, Show)

type Subst = MS.Map VarId Term


------------------------------------------------------------------------------
-- Terms
------------------------------------------------------------------------------

vars :: Term -> S.Set VarId
vars = \case
  Var v     -> S.singleton v
  App _c ts -> foldMap vars ts


------------------------------------------------------------------------------
-- Substitutions
------------------------------------------------------------------------------

empty :: Subst
empty = MS.empty

singleton :: VarId -> Term -> Subst
singleton = MS.singleton

apply :: Subst -> Term -> Term
apply s t = case t of
  Var v -> fromMaybe t $ MS.lookup v s
  App c ts -> App c $ map (apply s) ts

domain :: Subst -> S.Set VarId
domain = MS.keysSet

valid :: Subst -> Bool
valid s = S.null (domain s `S.intersection` foldMap vars (MS.elems s))


------------------------------------------------------------------------------
-- Unification
------------------------------------------------------------------------------

data UnificationFailure
  = DoesNotUnify
  | VarOccursInTerm VarId Term
  | DifferentConstructors Term Term
  | DifferentArity Term Term
  deriving (Eq, Show)

type UnifyM = StateT Subst (Except UnificationFailure)



mgu :: Term -> Term -> Either UnificationFailure Subst
mgu t1 t2 = runExcept $ execStateT (solve1 t1 t2) empty

{-
  | t1 == t2  = pure empty
  | otherwise =
      case (t1, t2) of
        (Var v1, _) -> elim v1 t2
        (_, Var v2) -> elim v2 t1
        (App c1 ts1, App c2 ts2) ->
          fold
          -- unless (c1 == c2) $ unificationFailure $ DifferentConstructors t1 t2
          -- unless (length ts1 == ts2) $ unificationFailure $
            -- DifferentArity t1 t2
  where
-}

solve1 :: Term -> Term -> UnifyM ()
solve1 ta0 tb0 = do
    -- apply the current solution to the terms
    s <- get
    let ta = apply s ta0
    let tb = apply s tb0
    -- see what equation there is to solve
    case (ta, tb) of
      (Var va, Var vb) | va == vb -> pure ()
      (Var va, _)                 -> elim va tb
      (_     , Var vb)            -> elim vb ta
      (App ca tsa, App cb tsb)    -> do
        ensure (ca == cb) $ DifferentConstructors ta tb
        ensure (length tsa == length tsb) $ DifferentArity ta tb
        forM_ (zip tsa tsb) (uncurry solve1)
  where
    ensure b err = unless b $ throwError err

-- | Eliminate a variable with a term provided the variable does not occur in
-- there.
elim :: VarId -> Term -> UnifyM ()
elim v t = do
    when (v `S.member` vars t) $ throwError $ VarOccursInTerm v t
    modify $ substExtend v t


substExtend :: VarId -> Term -> Subst -> Subst
substExtend v t = MS.insert v t . MS.map (apply (singleton v t))




------------------------------------------------------------------------------
-- Testing: Generators
------------------------------------------------------------------------------

-- todo: test empty generator

testVars :: [VarId]
testVars = ["a", "b"]


termWithVarsG :: [VarId] -> Gen Term
termWithVarsG vs = go
  where
    go = Gen.recursive Gen.choice
      [ Gen.element (App "Int" [] : map Var vs)
      ]
      [ Gen.subterm go (\t -> App "Maybe" [t])
      , Gen.subterm2 go go $ \t1 t2 -> App "Either" [t1, t2]
      ]

termG :: Gen Term
termG = termWithVarsG testVars

substG :: Gen Subst
substG = do
  domVars <- Gen.subsequence testVars
  let rangeVars = filter (not . (`elem` domVars)) testVars
  mappings <- forM domVars $ \v -> do
      t <- termWithVarsG rangeVars
      pure (v, t)
  pure $ MS.fromList mappings


unifiableG :: Gen (Term, Term, Subst)
unifiableG = do
    t1 <- termG
    t2 <- termG
    s  <- substG
    unless (apply s t1 == apply s t2) Gen.discard
    pure (t1, t2, s)


-- TODO: non-unifiable

------------------------------------------------------------------------------
-- Testing: Properties
------------------------------------------------------------------------------


prop_apply_empty :: Property
prop_apply_empty = property $ do
    t <- forAll termG
    apply empty t === t

prop_substG_valid :: Property
prop_substG_valid = property $ do
    s <- forAll substG
    assert (valid s)

prop_apply_idem :: Property
prop_apply_idem = property $ do
    t <- forAll termG
    s <- forAll substG
    apply s t === apply s (apply s t)

prop_mgu_valid :: Property
prop_mgu_valid = property $ do
    t1 <- forAll termG
    t2 <- forAll termG
    case mgu t1 t2 of
      Left _err -> pure ()
      Right s   -> do
        annotateShow s
        assert (valid s)

prop_mgu_result_unifies :: Property
prop_mgu_result_unifies = property $ do
    t1 <- forAll termG
    t2 <- forAll termG
    case mgu t1 t2 of
      Left _err -> pure ()
      Right s   -> apply s t1 === apply s t2


prop_mgu_unifiable_unifies :: Property
prop_mgu_unifiable_unifies = withDiscards 10000 $ property $ do
    (t1, t2, s0) <- forAll unifiableG
    case mgu t1 t2 of
      Left err -> annotateShow err >> failure
      Right s  -> do
        annotate ("original subst:" ++ show s0)
        annotate ("mgu subst:" ++ show s)
        apply s t1 === apply s t2


allTests :: Group
allTests = $$(discover)

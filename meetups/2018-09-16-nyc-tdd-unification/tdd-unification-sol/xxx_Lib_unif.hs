{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Lib where


import           Control.Monad

import           Data.Maybe
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HMS

import           Hedgehog hiding (Var)
-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range


------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------

-- | Constructor identifiers.
type ConId = T.Text

-- | Variable identifiers.
type VarId = T.Text

-- | Terms are built from variables and applications of constructors to terms.
data Term
  = Var VarId
  | App ConId [Term]
  deriving (Eq, Ord, Show)

type Subst = HMS.HashMap VarId Term

data UnificationFailure
  = MismatchingConstructors Term Term
  | MismatchingArities Term Term
  | RecursiveUnifyNotImplemented
  deriving (Eq, Ord, Show)


------------------------------------------------------------------------------
-- Substitution
------------------------------------------------------------------------------

empty :: Subst
empty = HMS.empty

singleton :: VarId -> Term -> Subst
singleton v t = HMS.singleton v t

apply :: Subst -> Term -> Term
apply subst t = case t of
  App con ts -> App con (map (apply subst) ts)
  Var v      -> fromMaybe t $ HMS.lookup v subst


------------------------------------------------------------------------------
-- Unification
------------------------------------------------------------------------------

unify :: Term -> Term -> Either UnificationFailure Subst
unify t1 t2 =
  case t1 of
    Var v1       -> pure $ singleton v1 t2
    App c1 args1 ->
      case t2 of
        Var v2       -> pure $ singleton v2 t1
        App c2 args2 -> do
          unless (c1 == c2) $ Left $ MismatchingConstructors t1 t2
          unless (null args1 && null args2) $ Left $ RecursiveUnifyNotImplemented
          pure empty




------------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------------

-- infrastructure
-----------------

unitTest :: PropertyT IO a -> Property
unitTest = withTests 1 . property . void

unifyTest :: (Term,  -> Property
unifyTest t1 t2 = unitTest $ do
    case unify t1 t2 of
      Left err -> annotateShow err >> failure
      Right subst -> apply subst t1 === apply subst t2


-- Unit tests
-------------

tZ :: Term
tZ = App "Z" []


-- tS :: Term -> Term
-- tS t = App "S" [t]
--
-- tD :: Term -> Term
-- tD t1 t2 = App "D" [t1, t2]

{-
(App ca [ta1,..,taN] =sigma= App cb [tb1,...,tbM]) <=>
(c1 == c2 && N == M && ta1 =sigma= tb1 && .. && taN == tbM)

v1 |-> t1, ..., vN |-> tN |- (Var v =sigma= t) <=>
-}



prop_tZ_unif_tZ :: Property
prop_tZ_unif_tZ = unifyTest tZ tZ

-- prop_va_unif_tSZ :: Property
-- prop_va_unif_tSZ = unifyTest va (tS tZ)
--
-- prop_va_unif_tSa :: Property
-- prop_va_unif_tSa = unifyTest va (tS va)


runTests :: IO Bool
runTests =
  checkParallel $$(discover)


type TestCase = ([(Term, Term)], Maybe [(Term, Term)])

testCaseToProperty :: TestCase  -> Property
testCaseToProperty tc@(eqs, mbSubst) = void $ property $ do
    annotateShow tc
    case (unify eqs, mbSubst) of
      (Left err, Just _) -> do
        annotate $ "unexpected failure:" ++ show err 
        failure
      (Left err, Nothing) -> pure ()
      (Right subst, Nothing) -> do
        annotate $ "unexpected success:" ++ show subst
        failure
      (Right subst, expectedSubst) -> 
        expectedSubst === subst
        forM_ eqs $ \(t1, t2) -> 
          apply subst t1 === apply subst t2

testCases :: [TestCase]
testCases =
  [ ([ a =?= a              ], Just [                     ] )
  , ([ a =?= b              ], Nothing)
  , ([ x =?= x              ], Just [                     ] )
  , ([ a =?= x              ], Just [ x |-> a             ] )
  , ([ x =?= y              ], Just [ x |-> y             ] )
  , ([ f[a,x] =?= f[a,b]    ], Just [ x |-> b             ] )
  , ([ f[a] =?= g[a]        ], Nothing)
  , ([ f[x] =?= f[y]        ], Just [ x |-> y             ] )
  , ([ f[x] =?= g[y]        ], Nothing)
  , ([ f[x] =?= f[y,z]      ], Nothing)
  , ([ f[g[x]] =?= f[y]     ], Just [ y |-> g[x]          ] )
  , ([ f[g[x],x] =?= f[y,a] ], Just [ x |-> a, y |-> g[a] ] )
  , ([ x =?= f[x]           ], Nothing)
  , ([ x =?= y, y =?= a     ], Just [ x |-> a, y |-> a    ] )
  , ([ a =?= y, x =?= y     ], Just [ x |-> a, y |-> a    ] )
  , ([ x =?= a, b =?= x     ], Nothing)
  ]
  where
    f = App "f"
    g = App "g"
    a = App "a" []
    b = App "b" []
    x = Var "x"
    y = Var "y"
    z = Var "z"
    (=?=) = (,)
    (|->) = (,)

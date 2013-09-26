{-# OPTIONS_GHC -Wall #-}
-- Mostly copied from
--
-- http://www.staff.science.uu.nl/~swier101/Papers/Theses/TopQuality.pdf


import           Control.Applicative
import           Control.Monad.State

import qualified Data.Set             as S
import qualified Data.Map             as M

import           Text.Parsec       hiding ((<|>))
import qualified Text.Parsec.Token    as PT
import qualified Text.Parsec.Language as PL

import           Text.PrettyPrint

newtype EIdent = EIdent String
    deriving (Show, Eq, Ord)

newtype TIdent = TIdent String
    deriving (Show, Eq, Ord)

data Expr
    = EVar EIdent
    | EApp Expr Expr
    | ELam EIdent Expr
    deriving (Show, Eq)

id_expr :: Expr
id_expr = ELam (EIdent "x") (EVar (EIdent "x"))

data Type
    = TVar TIdent
    | TFun Type Type
    deriving (Eq, Ord, Show)

id_type :: Type
id_type = TFun (TVar (TIdent "a")) (TVar (TIdent "a"))


free :: Type -> S.Set TIdent
free (TVar v)          = S.singleton v
free (TFun alpha beta) = free alpha `S.union` free beta

type Subst = M.Map TIdent Type


apply :: Subst -> Type -> Type
apply subst ty@(TVar v)       = M.findWithDefault ty v subst
apply subst (TFun alpha beta) = TFun (apply subst alpha) (apply subst beta)

type Env   = M.Map EIdent Type

type InferM a = StateT ([TIdent], [(Type, Type)]) (Either String) a

cannotInferType :: String -> InferM a
cannotInferType reason = lift $ Left reason

freshTVar :: InferM Type
freshTVar = do
    (tv:tvs, subst) <- get
    put (tvs, subst)
    return (TVar tv)

modifySubst :: Type -> InferM Type
modifySubst ty = do return ty

applySubst :: Type -> InferM Type
applySubst ty = do return ty
    -- (_, subst) <- get
    -- return (apply subst ty)

-- | Algorithm W
infer :: Env -> Expr -> InferM Type
infer env (EVar x) = case M.lookup x env of
    Nothing -> cannotInferType $ "Unknown identifier '" ++ show x ++ "'."
    Just ty -> return ty

infer env (ELam x e) = do
    beta  <- freshTVar
    tau   <- infer (M.insert x beta env) e
    beta' <- applySubst beta
    return $ TFun beta' tau

infer env (EApp e1 e2) = do
    tau1  <- infer env e1
    -- subst <- getSubst
    -- tau2  <- infer (M.map (apply subst) env) e2
    tau2  <- infer env e2
    tau1' <- applySubst tau1
    beta  <- freshTVar
    unify tau1' (TFun tau2 beta)
    applySubst beta

unify :: Type -> Type -> InferM ()
unify ty1 ty2 = do
    (tvs, eqs) <- get
    put (tvs, (ty1, ty2):eqs)



runInferM :: InferM a -> Either String (a, [(Type, Type)])
runInferM m = case runStateT m (someVariables, []) of
    Left reason           -> Left reason
    Right (res, (_, eqs)) -> Right (res, reverse eqs)
  where
    someVariables = map (TIdent . return) ['a'..'z']

inferType :: Expr -> Either String Doc
inferType e =
    ppResult <$> runInferM (infer M.empty e)
  where
    solveAll eqs = forM eqs $ \(ty1, ty2) -> solve ty1 ty2

    ppSol ty eqs = case execStateT (solveAll eqs) M.empty of
      Left msg    -> text $ "unsolvable: " ++ msg
      Right subst -> vcat $
        ppEqs [ (TVar v, ty') | (v, ty') <- M.toList subst ]
        ++
        [ text "", prettyType (apply subst ty) ]

    ppEqs eqs =
      [ prettyType ty1 <+> text " =?= " <+> prettyType ty2
      | (ty1, ty2) <- eqs
      ]

    ppResult (ty, eqs) = vcat $
      [prettyType ty, text "-- constraints --"] ++
      ppEqs eqs ++
      [text "-- solution --", ppSol ty eqs]


type UnifyM a = StateT Subst (Either String) a

solve :: Type -> Type -> UnifyM ()
solve ty1 ty2 = do
    subst <- get
    let ty1' = apply subst ty1
        ty2' = apply subst ty2

    case (ty1', ty2') of
      (TVar v1, TVar v2) | v1 == v2    -> return ()
                         | otherwise   -> elim v1 ty2'

      (TVar v1, _      )               -> elim v1 ty2'
      (_,       TVar v2)               -> elim v2 ty1'
      (TFun ty11 ty12, TFun ty21 ty22) -> do
          solve ty11 ty21
          solve ty12 ty22
      -- _ | ty1' == ty2'                 -> return ()
      --   | otherwise                    ->
      --         cannotUnify $ "Cannot unify '" ++ show ty1 ++
      --                       "' with '" ++ show ty2

  where
    elim :: TIdent -> Type -> UnifyM ()
    elim v ty
      | v `S.member` free ty =
          cannotUnify $ "Cannot eliminate '" ++ show (prettyType (TVar v)) ++
                            "' with '" ++ show (prettyType ty)
      | otherwise            =
          modify (M.insert v ty . M.map (apply (M.singleton v ty)))

    cannotUnify :: String -> UnifyM ()
    cannotUnify = lift . Left



------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

type Parser a = Parsec String () a

prettyType :: Type -> Doc
prettyType (TVar (TIdent x)) = text x
prettyType (TFun ty1 ty2)    = sep
  [ parens (prettyType ty1) <+> text "->"
  , parens (prettyType ty2)
  ]

parseExpr :: String -> Either String Expr
parseExpr =
    either (Left . show) Right .
    runParser (PT.whiteSpace PL.haskell *> expr) () dummySource
  where
    dummySource = "<interactive>"

    expr = foldl1 EApp <$> many1 atom

    atom = var <|> lambda <|> (PT.parens PL.haskell) expr

    identifier = EIdent <$> PT.identifier PL.haskell

    var = EVar <$> identifier

    lambda = do
        reservedOp "\\"
        ids <- many1 identifier
        reservedOp "."
        e <- expr
        return (foldr ELam e ids)


    reservedOp = PT.reservedOp PL.haskell


prettyExpr :: Expr -> Doc
prettyExpr (EVar (EIdent x))   = text x
prettyExpr (EApp e1 e2)        = sep [parens (prettyExpr e1), parens (prettyExpr e2)]
prettyExpr (ELam (EIdent x) e) = text ("\\" ++ x ++ ".") <+> prettyExpr e


------------------------------------------------------------------------------
-- Garbage heap
------------------------------------------------------------------------------


-- parseType :: String -> Either ParseError Expr
-- parseTyp =
--     runParser (PT.whiteSpace PL.haskell *> typ) () dummySource
--   where
--     dummySource = "<interactive>"
--
--     typ = foldr TFun <$> sepBy (reservedOp "-->") atom
--
--     atom = var <|> (PT.parens PL.haskell) typ
--
--     identifier = PT.identifier PL.haskell
--
--     var = EVar <$> identifier
--
--     lambda = do
--         reservedOp "\\"
--         ids <- many1 identifier
--         reservedOp "."
--         e <- expr
--         return (foldr ELam e ids)
--
--
--     reservedOp = PT.reservedOp PL.haskell



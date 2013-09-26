-- Purpose: Construct a simple interpreter for the Mini-Haskell language
-- Author:  Ben Bitwissle e.a.
-- Date:    2008 03 26
--
-- Extended with HM type inference by Simon Meier <iridcode@gmail.com>



import           Control.Arrow         (second)
import           Control.Monad.State

import           Data.Char
import           Data.List
import qualified Data.Map as M
import qualified Data.Set as S




-- **************************************************************************
-- ** MINI-HASKELL **********************************************************
-- **************************************************************************

-----------------------------------------------------------------------------
-- The term structure -------------------------------------------------------
-----------------------------------------------------------------------------

data Expr
    = Id String
    | Ap Expr Expr
    | Lam String Expr
    deriving (Show, Eq)


id_expr :: Expr
id_expr = Lam "x" (Id "x")



------------------------------------------------------------------------------
-- Type inference
------------------------------------------------------------------------------

data Type
    = TVar   String
    | TConst String
    | TApp   Type Type
    deriving (Eq, Ord, Show)

id_type :: Type
id_type = TApp (TApp (TConst "-->") (TVar "a")) (TVar "a")


free :: Type -> S.Set String
free (TVar x)       = S.singleton x
free (TConst _)     = S.empty
free (TApp ty1 ty2) = free ty1 `S.union` free ty2

apply1 :: String -> Type -> Type -> Type
apply1 x xTy ty = case ty of
    TVar   y | x == y    -> xTy
             | otherwise -> ty
    TConst _             -> ty
    TApp   ty1 ty2       -> TApp (apply1 x xTy ty1) (apply1 x xTy ty2)

type Subst = M.Map String Type

apply :: Subst -> Type -> Type
apply s ty0 = foldl' (\ty (x, xTy) -> apply1 x xTy ty) ty0 $ M.toList s


-- An inference monad
---------------------


type InferM = StateT ([String], Subst) (Either String)

type Env    = M.Map String Type

-- | Run a type-inference compuation.
runInferM :: InferM a -> Either String (a, Subst)
runInferM m = case runStateT m (map return ['a'..'z'], M.empty) of
    Left msg                -> Left msg
    Right (res, (_, subst)) -> Right (res, subst)

-- | Report a failure during type inference
cannotInferType :: String -> InferM a
cannotInferType msg = lift $ Left msg


-- | Generate a fresh type variable
freshTVar :: InferM Type
freshTVar = do
    (tv:tvs, tsubst) <- get
    put (tvs, tsubst)
    return (TVar tv)

-- | Access the current substitution.
getSubst :: InferM Subst
getSubst = gets snd

-- | Modify the current substitution.
modifySubst :: (Subst -> Subst) -> InferM ()
modifySubst f = modify (second f)

-- | Apply the current substitution to a type.
applySubst :: Type -> InferM Type
applySubst ty = do
    subst <- getSubst
    return (apply subst ty)


-- | Perform type inference under an environment for the free variables in the
-- term.
infer :: Env -> Expr -> InferM Type
infer env (Id i) = case M.lookup i env of
    Nothing  -> cannotInferType $ "Unknown identifier '" ++ i ++ "'."
    Just iTy -> return iTy   -- TODO: make fresh type

infer env (Lam x e) = do
    beta  <- freshTVar
    tau   <- infer (M.insert x beta $ M.delete x env) e
    beta' <- applySubst beta
    return $ TApp (TApp (TConst "-->") beta') tau

infer env (Ap e1 e2) = do
    tau1  <- infer env e1
    subst <- getSubst
    tau2  <- infer (M.map (apply subst) env) e2
    beta  <- freshTVar
    unify tau1 (TApp (TApp (TConst "-->") tau2) beta)
    applySubst beta


unify :: Type -> Type -> InferM ()
unify ty1 ty2 = do
    ty1' <- applySubst ty1
    ty2' <- applySubst ty2
    case (ty1' , ty2') of
      (TVar v1, TVar v2) | v1 == v2  -> return ()
                         | otherwise -> elim v1 ty2
      (TVar v1, _      )             -> elim v1 ty2
      (_,       TVar v2)             -> elim v2 ty1
      (TApp t11 t12, TApp t21 t22)   -> unify t11 t21 >> unify t12 t22
      _  | ty1' == ty2' -> return ()
         | otherwise    -> cannotInferType $
             "Cannot unify '" ++ show ty1 ++ "' with '" ++ show ty2 ++ "'."
  where
    elim v ty
      | v `occurs` ty =
          cannotInferType $ "Cannot unify '" ++ v ++ "' with '" ++ show ty ++ "'"
      | otherwise     = modifySubst $ M.insert v ty . M.map (apply1 v ty)

    occurs :: String -> Type -> Bool
    occurs v ty = v `S.member` free ty


testInfer :: Expr -> Either String (Type, Subst)
testInfer e = runInferM $ infer M.empty e





------------------------------------------------------------------------------
-- some lambda terms to play with:
------------------------------------------------------------------------------

s = "(%x y z. x z (y z))"
k = "(%x y. x)"
i = "(%x. x)"

add = "(%x y s z. x s (y s z))"
mul = "(%x y s z. x (y s) z)"
mul' = "(%x y s. x (y s))"   -- eta-conversion; i.e. partial application
exp' = "(%x y. y x)"

zero  = "(%s z. z)"
one   = "(%s z. s (z))"
two   = "(%s z. s (s z))"
three = "(%s z. s (s (s z)))"

-- example: calculating 2 + 3 in the lambda calculus:
--
--   evalStepwiseE (add++two++three) ~~>
--     (%x y s z. x s (y s z)) (%s z. s (s z)) (%s z. s (s (s z))) =
--     (%y s z. (%s z. s (s z)) s (y s z)) (%s z. s (s (s z))) =
--     (%y s z. (%z. s (s z)) (y s z)) (%s z. s (s (s z))) =
--     (%y s z. s (s (y s z))) (%s z. s (s (s z))) =
--     %s z. s (s ((%s z. s (s (s z))) s z)) =
--     %s z. s (s ((%z. s (s (s z))) z)) =
--     %s z. s (s (s (s (s z))))
--
--   couting the number of 's' shows us that 2+3 is indeed 5 ;-)

mini_true = "(%t f. t)"
mini_false = "(%t f. f)"

mini_and = "(%p q. p q p)"
mini_or  = "(%p q. p p q)"
mini_not = "(%p t f. p f t)"
mini_if  = "(%p.p)"

iszero = "(% n. n (%x. (%t f. f)) (%t f. t))"

--------------------------------------------------
-- the terms needed for the last exercise sheet --
--------------------------------------------------

termF = "(%x. x (%y. x y))"
termG = "(%x. y x)"
termS = "(%x y z. x z (y z))"
termK = "(%x y. x)"
termC = "(%x f g. f (g x))"

-- Some more exercises:
--
-- try to do this first by hand and check then using:
--    for lazy:   evalL ex1
--                evalStepwiseL ex1
--    for eager:  evalE ex1
--                evalStepwiseE ex1

ex1  = "((%x y. y x z) z) (%y z. (%x. y y) z)"
ex15 =  "(%x y z. (%a b. b a c) b (%x. y z))"
ex2  = "(%x y z. x ((%a x. y x) b x)) (%x. x) (%b. (%z. b z) a)"




-- ***************************************************************************
-- ** PARSER COMBINATORS *****************************************************
-- ***************************************************************************


-- NOTE: The development of some combinator library for parsers is not
-- difficult per se. However, making a production quality combinator
-- library involves a lot of thinking and optimization. This has already
-- been done as part of the "Parsec" library. In a recent GHC distribution
-- this library can be used by
--
--   import Text.ParserCombinators.Parsec
--
-- for its documentation ask Hoogle with "Parsec"



-- parsers are encapsulated in a new datatype in order to
-- guarantee type-safety (and to enable making it a Monad instance)
data Parser a = Prs (String -> [(a,String)])

-- apply a parser to a string
parse :: Parser a -> String -> [(a,String)]
parse (Prs p) inp = p inp

-- apply a parser and return the value of the first complete parse
-- NOTE: We could use Maybe to defer error handling
completeParse :: Parser a -> String -> a
completeParse p inp
  | null results = error "Parse unsuccessful"
  | otherwise    = head results
  where results = [val | (val,[]) <- parse p inp]

-- In order to support the "do"-notation, we have to make
-- our Parser type an instance of the Monad typeclass
instance Monad Parser where
    -- return :: a -> Parser a
    --
    -- Do not consume any input and return a fixed result.
    return x = Prs (\inp -> [(x,inp)])

    -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    --
    -- Sequence a parser with a parser generator. The parser
    -- generator generates a parser depending on the outcome
    -- of the first parser. The generated parser is then applied
    -- to the input.
    p >>= gen = Prs (\inp -> [ (val',out') | (val,out) <- parse p inp,
                                             (val',out') <- parse (gen val) out ])

    -- fail :: String -> Parser a
    --
    -- every monad supports the method fail, which signals an error
    -- described by a string. As we cannot store an error value in
    -- our parser type, we just ignore it and signal a simple failure.
    fail = const failure


--------------------------------------------------------------------------

-- choice combinator
(|||) :: Parser a -> Parser a -> Parser a
p ||| q = Prs (\inp -> (parse p inp) ++ (parse q inp))

-- repetition
many :: Parser a -> Parser [a]    -- 0 or more repetitions of p
many p = many1 p ||| return []

many1 :: Parser a -> Parser [a]
many1 p = do val <- p               -- 1 or more repetitions of p
             vals <- many p
             return (val:vals)

--------------------------------------------------------------------------

-- some simple parsers
failure = Prs (\inp -> [])          -- the parser that always fails

item :: Parser Char
item = Prs (\inp -> case inp of
                    "" -> []
                    (x:xs) -> [(x,xs)])

sat  :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure

char  :: Char -> Parser Char
char x = sat (==x)

string       :: String -> Parser String
string ""     = return ""
string (x:xs) = do char x
                   string xs
                   return (x:xs)


-- some auxiliary parsers
spaces = many (sat (==' '))

identifier = do spaces
                id <- many1 (sat isIdentLetter)
                spaces
                return id
             where isIdentLetter c = isLetter c || (c == '\'')

token s = do spaces
             t <- string s
             spaces
             return t

-----------------------------------------------------------------------------
-- Pretty Printing our lambda expressions -----------------------------------
-----------------------------------------------------------------------------

-- for comparison: the old simple pretty printer
prettyPrint' (Id s)    = s
prettyPrint' (Lam s t) = "(%" ++ s ++ ". " ++ (prettyPrint' t) ++ ")"
prettyPrint' (Ap s t)  = "(" ++ (prettyPrint' s) ++ " " ++ (prettyPrint' t) ++ ")"

-- We adapt this pretty printer now such that as few as possible '%' and
-- parentheses are printed.

-- helper function: prints parentheses around the term t if cond is true
parens :: Bool -> String -> String
parens cond t = if cond then "("++t++")" else t

-- a nice pretty print function for our lambda expressions
prettyPrint (Id x)   = x
prettyPrint (Ap x y) = left++" "++right
  where left  = parens (isLam x)      (prettyPrint x)
        right = parens (not (isId y)) (prettyPrint y)

        isId (Id _) = True
        isId _      = False

        isLam (Lam _ _) = True
        isLam _         = False

prettyPrint (Lam x y) = left++right y
  where left = "%" ++ x
        right (Lam _ _) = " " ++ tail (prettyPrint y) -- remove "%" and append
        right _         = ". " ++ prettyPrint y

-- pretty prints a sequence of expressions as a series of equivalences
prettyPrintSequence xs = foldl1 (\x y -> x ++ " = \n" ++ y) (map prettyPrint xs)


-----------------------------------------------------------------------------
-- Parsing Lambda Exprs -----------------------------------------------------
-----------------------------------------------------------------------------
--
-- grammar:
--   Expr  ::= Ident Expr* | Lamb Expr* | Paren Expr*
--   Paren ::= ( Expr )
--   Lamb  ::= % Ident+ . Expr


atom  = ident ||| lamb ||| paren

ident = do id <- identifier
           return (Id id)

term  = do t <- atom                 -- left associative apply
           ts <- many atom
           return (foldl Ap t ts)

lamb  = do token "%"
           ids <- many1 identifier
           token "."
           t <- term
           return (foldr Lam t ids)

paren = do token "("
           t <- term
           token ")"
           return t

str2term = completeParse term



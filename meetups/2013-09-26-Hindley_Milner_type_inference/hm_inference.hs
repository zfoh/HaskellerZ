
import           Control.Applicative
import           Control.Monad

import           Text.Parsec       hiding ((<|>))
import qualified Text.Parsec.Token    as PT
import qualified Text.Parsec.Language as PL

import           Text.PrettyPrint


data Expr
    = EVar String
    | EApp Expr Expr
    | ELam String Expr
    deriving (Show, Eq)


type Parser a = Parsec String () a

expr :: Parser Expr
expr =
    foldl1 EApp <$> many1 atom
  where

    atom = var <|> lambda <|> (PT.parens PL.haskell) expr

    identifier = PT.identifier PL.haskell

    var = EVar <$> identifier

    lambda = do
        reservedOp "\\"
        ids <- many1 identifier
        reservedOp "."
        e <- expr
        return (foldr ELam e ids)


    reservedOp = PT.reservedOp PL.haskell

parseExpr :: String -> Either ParseError Expr
parseExpr =
    runParser (PT.whiteSpace PL.haskell *> expr) () dummySource
  where
    dummySource = "<interactive>"


prettyExpr :: Expr -> Doc
prettyExpr (EVar x)     = text x
prettyExpr (EApp e1 e2) = sep [parens (prettyExpr e1), parens (prettyExpr e2)]
prettyExpr (ELam x e)   = text ("\\" ++ x ++ ".") <+> prettyExpr e



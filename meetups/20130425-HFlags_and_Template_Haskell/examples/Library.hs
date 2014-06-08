{-# LANGUAGE TemplateHaskell #-}

module Library where

import Language.Haskell.TH

data MyLanguage = MLInt Integer
                | Add MyLanguage MyLanguage

compiler :: MyLanguage -> Q Exp
compiler (MLInt x) = [| x :: Integer |]
compiler (Add e1 e2) = do
  me1 <- compiler e1
  me2 <- compiler e2
  return $ InfixE (Just me1) (VarE '(+)) (Just me2)

{-# LANGUAGE TemplateHaskell #-}

module Tup where

import Control.Monad
import Language.Haskell.TH

pprintQ q = putStrLn . pprint =<< runQ q

get n k = do
  when (n <= 0 || k < 0 || k >= n) $ reportError "index out of bounds"
  basic <- runQ [| \(x, _) -> x |]
  let LamE [tup] body = basic
  let TupP [var, wild] = tup
  let before = replicate k wild
  let after = replicate (n - k - 1) wild
  return $ LamE [TupP (before ++ [var] ++ after)] body

get2 n k = do
  when (n <= 0 || k < 0 || k >= n) $ reportError "index out of bounds"
  x <- newName "x"
  let body = varE x
  let param = tupP $ replicate k wildP ++ [varP x] ++ replicate (n - k - 1) wildP
  lamE [param] body

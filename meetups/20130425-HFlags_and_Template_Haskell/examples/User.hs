{-# LANGUAGE TemplateHaskell #-}

module User where

import Library
import Language.Haskell.TH

expr :: Q Exp
expr = compiler $ Add (MLInt 12) (Add (MLInt 34) (MLInt 56))

value :: Integer
value = $(compiler $ Add (MLInt 12) (Add (MLInt 34) (MLInt 56)))

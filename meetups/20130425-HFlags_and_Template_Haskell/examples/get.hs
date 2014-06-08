{-# LANGUAGE TemplateHaskell #-}

import Language.Haskell.TH
import Control.Monad

-- $(get 6 3) (1,2,3,4,5,6)          => 3
get :: Int -> Int -> Q Exp
get n k = do
  when (k > n) $ reportError "k > n"
  var <- newName "x"
  let wildPsAtStart = replicate (k - 1) WildP
  let wildPsAtEnd = replicate (n - k) WildP
  let list = wildPsAtStart ++ (VarP var:wildPsAtEnd)
  return $ LamE [TupP list] (VarE var)

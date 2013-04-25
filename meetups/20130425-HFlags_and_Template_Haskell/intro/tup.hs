{-# LANGUAGE TemplateHaskell #-}

import Tup

-- :set -ddump-splices

main = print ($(get2 6 3) (0,1,2,3,4,5))
  where
    -- :set -fwarn-name-shadowing
    x = 42

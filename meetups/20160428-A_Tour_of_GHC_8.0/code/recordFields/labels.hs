-- Basic tests of overloaded labels

{-# LANGUAGE OverloadedLabels
           , DataKinds
           , FlexibleContexts
           , FlexibleInstances
           , MultiParamTypeClasses
           , NoMonomorphismRestriction
  #-}

import GHC.OverloadedLabels

instance IsLabel "true" Bool where
  fromLabel _ = True

instance IsLabel "true" Int where
  fromLabel _ = 1

instance IsLabel "false" Bool where
  fromLabel _ = False

a :: Bool
a = #false

b :: IsLabel "true" t => t
b = #true


main = do print a
          print (b :: Bool)
          print (b :: Int)

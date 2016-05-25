-- Source: https://github.com/khibino/haskell-relational-record/issues/19

module HRR.Crashing where

import Data.Int (Int32)
import Database.Relational.Query

zero :: Relation () Int32
zero = relation $ return (value 0)

captureVariable :: Projection Flat Int32 -> Relation () Int32
captureVariable i = relation $ do
  z <- query zero
  wheres (i .=. z)
  return z

foo :: Relation () (Int32, Int32)
foo = relation $ do
  i <- query zero
  j <- query (captureVariable i)

  return ((,) |$| i ! id'
              |*| j ! id')

crash :: IO ()
crash = putStrLn $ show foo ++ ";"

-- Run this and try it on psql

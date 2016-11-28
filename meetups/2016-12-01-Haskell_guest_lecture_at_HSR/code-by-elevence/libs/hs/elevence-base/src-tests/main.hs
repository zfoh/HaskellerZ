{-# LANGUAGE NoImplicitPrelude #-}

-- | Tests for all modules in @elevence-base@.
module Main (main) where

import qualified Data.Relation.Binary.Ordered.Tests
import qualified Data.List.Extended.Tests

import           Elevence.Prelude

import qualified Test.Tasty             as Tasty


-- | Run all tests.
main :: IO ()
main = Tasty.defaultMain tests

-- | All tests for the @elevence-base@ library.
tests :: Tasty.TestTree
tests = Tasty.testGroup "elevence-base"
    [ Data.List.Extended.Tests.tests
    , Data.Relation.Binary.Ordered.Tests.tests
    ]

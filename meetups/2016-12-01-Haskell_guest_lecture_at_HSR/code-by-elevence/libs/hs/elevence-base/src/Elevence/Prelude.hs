{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
-- | Custom Prelude without partial functions and a more extensive default
-- export list.
module Elevence.Prelude
  (
    -- * Prelude without partial functions
    module Prelude

    -- ** Total variants of partial functions

    -- TODO (SM): switch to 'located-base' for the 'xxxNote' versions
  , atMay        , atDef        , atNote
  , tailMay      , tailDef      , tailNote
  , initMay      , initDef      , initNote
  , headMay      , headDef      , headNote
  , lastMay      , lastDef      , lastNote
  , minimumMay   , minimumDef   , minimumNote
  , maximumMay   , maximumDef   , maximumNote
  , minimumByMay , minimumByDef , minimumByNote
  , maximumByMay , maximumByDef , maximumByNote
  , foldr1May    , foldr1Def    , foldr1Note
  , foldl1May'   , foldl1Def'   , foldl1Note'
  , scanl1May    , scanl1Def    , scanl1Note
  , scanr1May    , scanr1Def    , scanr1Note
  , assertNote
  , readMay      , readDef      , readNote
                 , fromJustDef  , fromJustNote

    -- * Common exports from Control.*
  , module Control.Applicative
  , module Control.Monad

    -- * Common exports from Data.*
  , module Data.Char
  , module Data.Function
  , module Data.List.Extended
  , module Data.Maybe
  , module Data.Ord
  , module Data.Proxy
  , module Data.Tuple
  , module Data.Void

  , partitionEithers

  , Data
  , Hashable

    -- ** Monoid
  , Monoid(..)
  , (<>)
  , when_
  , unless_

#if !MIN_VERSION_base(4,8,0)
    -- ** Foldable
  , Foldable
  , foldMap

     -- ** Traversable
  , Traversable
  , traverse
#endif

    -- ** Tagged values
  , Tagged(Tagged, unTagged)

    -- ** GHC Generics support
  , Generic

    -- ** Make default instances
  , makeInstances
  , makeInstancesExcept
  ) where


-- orphan instances (all of them must be brought into scope here)
-----------------------------------------------------------------

import Orphans.Lib_base ()
import Orphans.Lib_aeson ()
import Orphans.Lib_hashable ()
import Orphans.Lib_parsec ()


-- normal imports
-----------------

import Control.Applicative
import Control.Exception
import Control.Monad

import Data.Char
import Data.Data   (Data)
import Data.Either (partitionEithers)

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable    (Foldable,    foldMap)
import Data.Traversable (Traversable, traverse)
#endif

import Data.Function
import Data.Hashable (Hashable)
import Data.List.Extended hiding
  (
    -- hide partial functions
    minimumBy
  , maximumBy
  , foldl1
  , foldl1'

    -- foldl is almost always the wrong choice performance-wise.
  , foldl
  )
import Data.Maybe


#if MIN_VERSION_base(4,8,0)
import Data.Monoid.Extended ((<>), when_, unless_)
#else
import Data.Monoid.Extended (Monoid(..), (<>), when_, unless_)
#endif

import Data.Ord
import Data.Proxy
import Data.Tagged          (Tagged(Tagged, unTagged))
import Data.Tuple
import Data.Void

import GHC.Generics (Generic)

import Prelude hiding
  (
    -- hide partial functions
    tail
  , init
  , head
  , last
  , minimum
  , maximum
  , foldr1
  , scanl1
  , scanr1
  , read

    -- foldl is almost always the wrong choice performance-wise.
  , foldl
  )

-- Total variants of partial functions. See above for the collection.
import Safe


-- qualified imports
--------------------

import qualified Language.Haskell.TH        as TH
import qualified Data.Aeson                 as Aeson


------------------------------------------------------------------------------
-- Support for simplified instance declarations via template Haskell
------------------------------------------------------------------------------

-- NOTE (SM): this code is an experiment that aims to provide a single place
-- for declaring what instances our datatypes support by default.


-- | One TH line of deriving our usual instances (ToJSON, FromJSON, Generic, Eq, Show, Ord)
makeInstances :: TH.Name -> TH.Q [TH.Dec]
makeInstances = makeInstancesExcept []

makeInstancesExcept :: [TH.Name] -> TH.Name -> TH.Q [TH.Dec]
makeInstancesExcept exceptions name = do
    info <- TH.reify name
    case info of
      TH.TyConI (TH.DataD    [] _name []       _constructors _deriving) -> arg0
      TH.TyConI (TH.NewtypeD [] _name []       _constructors _deriving) -> arg0
      TH.TyConI (TH.DataD    [] _name [ _var ] _constructors _deriving) -> arg1
      TH.TyConI (TH.NewtypeD [] _name [ _var ] _constructors _deriving) -> arg1
      info' -> do
        TH.reportError $ "makeInstances can't currently handle type definition like this: " ++ show info'
        return []
  where
    except f x = if elem x exceptions then [] else [ f x ]

    -- no type arguments, e.g.:
    -- instance FromJSON X
    -- instance ToJSON X
    arg0 = do
      let genD t = TH.standaloneDerivD
                     (return [])                          -- no predicates
                     (TH.appT (TH.conT t) (TH.conT name))
      let genI t = TH.instanceD
                     (return [])                          -- no predicates
                     (TH.appT (TH.conT t) (TH.conT name)) -- ToJSON X
                     []                                   -- no implementation, default is good
      sequence $ concat
               [ except genI ''Aeson.ToJSON
               , except genI ''Aeson.FromJSON
               , except genD ''Eq
               , except genD ''Ord
               , except genD ''Show
               , except genD ''Generic
               ]

    -- there is one type argument which is also jsonified already, e.g.:
    -- instance FromJSON a => FromJSON (X a)
    -- instance ToJSON a   => ToJSON (X a)
    arg1 = do
      let genD needsPred t =
            do
              var <- TH.newName "arg"
              TH.standaloneDerivD
                (sequence (if needsPred then [TH.appT (TH.conT t) (TH.varT var)]
                                        else []))
                (TH.appT (TH.conT t) (TH.appT (TH.conT name) (TH.varT var)))
      let genI t =
            do
              var <- TH.newName "arg"
              TH.instanceD
                (sequence [TH.appT (TH.conT t) (TH.varT var)])               -- ToJSON a
                (TH.appT (TH.conT t) (TH.appT (TH.conT name) (TH.varT var))) -- ToJSON (X a)
                []                                                           -- no implementation, default is good
      sequence $ concat
               [ except genI ''Aeson.ToJSON
               , except genI ''Aeson.FromJSON
               , except (genD True) ''Eq
               , except (genD True) ''Ord
               , except (genD True) ''Show
               , except (genD False) ''Generic
               ]

{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module OpaleyeDemo.Ids where

import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Opaleye                    (Column, Nullable, PGInt4, PGOrd,
                                             PGText)

--------------------------------------------------------------------------------
-- | Todo Id
data TodoId' a = TodoId { todoId :: a } deriving Show

makeAdaptorAndInstance "pTodoId" ''TodoId'

type TodoId = TodoId' Int
type TodoIdColumn = TodoId' (Column PGInt4)
type TodoIdColumnMaybe = TodoId' (Maybe (Column PGInt4))
type TodoIdColumnNullable = TodoId' (Column (Nullable PGInt4))

--------------------------------------------------------------------------------
-- | Priority indicator
data Prio' a = Prio { prio :: a } deriving Show

makeAdaptorAndInstance "pPrio" ''Prio'

type Prio = Prio' (Maybe Int)
type PrioColumn = Prio' (Column (Nullable PGInt4))

--------------------------------------------------------------------------------
-- | Hashtag
data HashtagStr' a = HashtagStr { hashtagStr :: a } deriving Show

makeAdaptorAndInstance "pHashtagStr" ''HashtagStr'

type HashtagStr = HashtagStr' String
type HashtagStrColumn = HashtagStr' (Column PGText)
type HashtagStrColumnNullable = HashtagStr' (Column (Nullable PGText))

--------------------------------------------------------------------------------
-- | This is already available in 'master' but no on hackage
-- | so please forgive this hack :)
instance PGOrd a => PGOrd (Nullable a)


{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module HRR.Todo
    ( -- * Exports
      Todo(..)
    , PiTodo(..)
    , piTodo'
    , id'
    , dueDate'
    , title'
    , prio'
    , todo
    , todosByPriority
    , todosByPriorityAndBeforeDate
    , todoIdAndTitleByPriorityAndBeforeDate
    , tableOfTodo
    , insertTodo
    , insertQueryTodo
    , selectTodo
    , updateTodo
    , piTodoDate'
    , piTodoTitle'
    , piTodoPrio'
    ) where

import           Data.Time.Calendar        (Day)
import           Database.HDBC.Query.TH    (makeRecordPersistableDefault)
import           Database.Relational.Query hiding (id')
import           GHC.Int                   (Int32)
import           HRR.DataSource
import           Prelude                   hiding (id)


-- | Keep in mind this will turn all the fields in the table into a
-- | CamelCase
--
-- | https://github.com/khibino/haskell-relational-record/blob/master/names-th/src/Language/Haskell/TH/Name/CamelCase.hs
--
-- | Why they didn't use Text.Inflections.Camelize I still don't know
--

$(defineTable "public" "todo" [''Show])

todosByPriority :: Relation () Todo
todosByPriority = relation $ do
    t <- query todo
    desc $ t ! prio'
    return t

todosByPriorityAndBeforeDate :: Relation Day Todo
todosByPriorityAndBeforeDate = relation' . placeholder $ \ph -> do
    t <- query todosByPriority
    wheres $ t ! dueDate' .<=. ph
    return t

todoIdAndTitleByPriorityAndBeforeDate :: Relation Day (Int32, String)
todoIdAndTitleByPriorityAndBeforeDate = relation' $ do
    (ph, t) <- query' todosByPriorityAndBeforeDate
    return (ph, t ! id' >< t ! title')

instance Eq Todo where
    t1 == t2 = id t1 == id t2

data PiTodo = PiTodo { piTodoTitle :: String
                     , piTodoDate  :: Day
                     , piTodoPrio  :: Maybe Int32
                     }

$(makeRecordPersistableDefault ''PiTodo) -- Creates the `Pi` fields

-- Creates a "bindable" placeholder for insertions
piTodo' :: Pi Todo PiTodo
piTodo' = PiTodo |$| title'
                 |*| dueDate'
                 |*| prio'

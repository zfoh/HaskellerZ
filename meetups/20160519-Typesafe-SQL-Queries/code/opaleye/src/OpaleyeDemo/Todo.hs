{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

--------------------------------------------------------------------------------
-- | See TodoManual.hs to avoid using TemplateHaskell, though you should be
-- | aware that this will make you write more boilerplate code
module OpaleyeDemo.Todo where

import           Control.Arrow              (returnA, (>>>))
import           Data.Int                   (Int64)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import           Data.Time.Calendar         (Day)
import           Database.PostgreSQL.Simple (Connection)
import           Opaleye                    (Column, PGDate, PGText, Query,
                                             Table (..), descNullsLast,
                                             keepWhen, optional, orderBy,
                                             pgInt4, queryTable, required,
                                             restrict, runDelete,
                                             runInsertReturning, (.==), (.===))
import           OpaleyeDemo.Ids

--------------------------------------------------------------------------------
-- | Todo declaration
data Todo' i t d p = Todo { _id      :: i
                          , _title   :: t
                          , _dueDate :: d
                          , _prio    :: p
                          } deriving Show

makeAdaptorAndInstance "pTodo" ''Todo'

{-

The instance generated will be

instance (ProductProfunctor p, Default p a1_0 a1_1,
          Default p a2_0 a2_1, Default p a3_0 a3_1, Default p a4_0 a4_1) =>
          Default p (Todo' a1_0 a2_0 a3_0 a4_0) (Todo' a1_1 a2_1 a3_1 a4_1)

and pTodo will have the type

pTodo
  :: ProductProfunctor p =>
     Todo' (p a1_0 a1_1) (p a2_0 a2_1) (p a3_0 a3_1) (p a4_0 a4_1)
     -> p (Todo' a1_0 a2_0 a3_0 a4_0) (Todo' a1_1 a2_1 a3_1 a4_1)

-}

--------------------------------------------------------------------------------
-- | Todo Columns
type TodoColumns
  = Todo' TodoIdColumn (Column PGText) (Column PGDate) PrioColumn
type TodoInsertColumns
  = Todo' TodoIdColumnMaybe (Column PGText) (Column PGDate) PrioColumn
type Todo
  = Todo' TodoId String Day Prio

--------------------------------------------------------------------------------
-- | Functions
todoTable :: Table TodoInsertColumns TodoColumns
todoTable = Table "todos" $ pTodo Todo
    { _id      = pTodoId . TodoId $ optional "id"
    , _title   = required "title"
    , _dueDate = required "due_date"
    , _prio    = pPrio . Prio $ required "prio"
    }

todoQuery :: Query TodoColumns
todoQuery = queryTable todoTable

todosByPriority :: Query TodoColumns
todosByPriority = orderBy (descNullsLast $ prio . _prio) todoQuery

selectTodo :: TodoId -> Query TodoColumns
selectTodo tid = proc () -> do
    todos    <- todoQuery -< ()
    restrict -< todoId (_id todos) .== pgInt4 (todoId tid)
    returnA  -< todos

selectTodo' :: TodoId -> Query TodoColumns
selectTodo' tid = todoQuery >>> keepWhen
                  (\todos -> todoId (_id todos) .== pgInt4 (todoId tid))

insertTodo :: Connection -> TodoInsertColumns -> IO TodoId
insertTodo conn t = fmap head (runInsertReturning conn todoTable t _id)

deleteTodo :: Connection -> TodoId -> IO Int64
deleteTodo conn tid = runDelete conn todoTable
                      (\t -> todoId (_id t) .=== (pgInt4 . todoId) tid)

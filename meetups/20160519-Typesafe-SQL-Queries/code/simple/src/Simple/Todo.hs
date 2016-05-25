{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

-- QuasiQuotes refresher => QuasiQuotes are not like TemplateHaskell's quotes,
-- rather they are more like TH splices. See mailing list
-- https://mail.haskell.org/pipermail/haskell-cafe/2016-April/123619.html

module Simple.Todo
    ( -- * Exports
      Todo (..)
    , isNew
    , allTodos
    , findTodo
    , deleteTodo
    , addTodo
    , allTodosByDate
    , allTodosByPrio
    , allLateTodos
    ) where

import           Data.Maybe                         (isNothing)
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromRow (field, fromRow)
import           Database.PostgreSQL.Simple.SqlQQ   (sql)
import           Database.PostgreSQL.Simple.Time    (Date)
import           Database.PostgreSQL.Simple.ToField (toField)
import           Database.PostgreSQL.Simple.ToRow   (toRow)
import           GHC.Int                            (Int64)

data Todo = Todo { getId      :: !(Maybe Int) -- Can be null
                 , getTitle   :: !String      -- Title of the todo
                 , getDueDate :: !Date        -- Date of the todo
                 , getPrio    :: !(Maybe Int) -- Priority of the todo
                 } deriving (Show)

instance FromRow Todo where
    fromRow = Todo <$> field -- id
                   <*> field -- title
                   <*> field -- due date
                   <*> field -- prio

instance ToRow Todo where
    toRow t = [ toField . getTitle $ t
              , toField . getDueDate $ t
              , toField . getPrio $ t
              ]

instance Eq Todo where
    t1 == t2 = getId t1 == getId t2

isNew :: Todo -> Bool
isNew t = isNothing (getId t)

allTodos :: Connection -> IO [Todo]
allTodos conn = query_ conn q
                where
                  q = [sql| select id, title, due_date, prio
                            from todos |]

findTodo :: Connection -> Int -> IO (Maybe Todo)
findTodo conn tid = do
               result <- query conn q (Only tid)
               return $ if length result == 1 then
                    Just (head result)
               else
                    Nothing
               where
                 q = [sql| select id, title, due_date, prio
                           from todos
                           where id = ? |]

deleteTodo :: Connection -> Int -> IO Int64
deleteTodo conn tid = execute conn q (Only tid)
                      where
                        q = [sql| delete from todos
                                  where id = ? |]

addTodo :: Connection -> Todo -> IO (Only Int)
addTodo conn t = head <$> query conn q t
                 where
                   q = [sql| insert into todos (title, due_date, prio)
                             values (?, ?, ?)
                             returning id |]

allTodosByDate :: Connection -> Date -> IO [Todo]
allTodosByDate conn d = query conn q (Only d)
                        where
                          q = [sql| select id, title, due_date, prio
                                    from todos
                                    where due_date = ? |]

allTodosByPrio :: Connection -> IO [Todo]
allTodosByPrio conn = query_ conn q
                      where
                        q = [sql| select id, title, due_date, prio
                                  from todos
                                  order by prio desc
                                  nulls last |]

allLateTodos :: Connection -> IO [Todo]
allLateTodos conn = query_ conn q
                    where
                      q = [sql| select id, title, due_date, prio
                                from todos
                                where due_date < current_date |]


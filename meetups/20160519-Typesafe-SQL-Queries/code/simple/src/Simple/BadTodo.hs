{-# LANGUAGE OverloadedStrings #-}

module Simple.BadTodo
    ( -- * Exports
      BadTodo (..)
    , allBadTodos -- ^ This function will fail!
    ) where

-- | This data is defined wrongly on purpose, for
-- | presentation purposes
data BadTodo = BadTodo !(Maybe Int) -- ^ Can be null
                       !String      -- ^ Title of the todo
                       !String      -- ^ Date of the todo
                       !(Maybe Int) -- ^ Priority of the todo
                       deriving (Show)

instance FromRow BadTodo where
    fromRow = BadTodo <$> field -- ^ id
                      <*> field -- ^ title
                      <*> field -- ^ due date
                      <*> field -- ^ prio

-- | This will throw an Exception: on purpose
allBadTodos :: Connection -> IO [BadTodo]
allBadTodos conn = query_ conn q
                where
                  q = [sql| select id, title, due_date, prio
                            from todos |]


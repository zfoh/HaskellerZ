{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module OpaleyeDemo.Utils
    ( -- * Exports
      printSql
    , runQueryDebug
    , runQ
    , todoToString
    ) where

import           Data.List                       (intercalate)
import           Data.Profunctor.Product.Default (Default)
import           Database.PostgreSQL.Simple      (Connection)
import           Opaleye                         (Query, QueryRunner,
                                                  Unpackspec, runQuery,
                                                  showSqlForPostgres)
import           OpaleyeDemo.Flags
import qualified OpaleyeDemo.Hashtag             as H
import qualified OpaleyeDemo.Ids                 as I
import qualified OpaleyeDemo.Todo                as T

printSql :: Default Unpackspec a a => Query a -> IO ()
printSql = putStrLn . showSqlForPostgres

runQueryDebug :: (Default Unpackspec columns columns,
                  Default QueryRunner columns haskells) =>
                  Connection -> Query columns -> IO [haskells]
runQueryDebug c q = printSql q >> runQuery c q

runQ :: (Default Unpackspec columns columns,
         Default QueryRunner columns haskells) =>
         Connection -> Query columns -> [Flag] -> IO [haskells]
runQ c q flags = if Debug `elem` flags then
                    runQueryDebug c q
                 else
                    runQuery c q

todoToString :: T.Todo -> [H.Hashtag] -> String
todoToString todo hashtags
  = unwords [ show ((I.todoId . T._id) todo) ++ "."
            , T._title todo
            , "(due by: " ++ show (T._dueDate todo) ++ ")"
            , "(prio: " ++ maybe "-" show (I.prio (T._prio todo)) ++ ")"
            , intercalate ", " (map (I.hashtagStr . H._hashtag) hashtags)
            ]


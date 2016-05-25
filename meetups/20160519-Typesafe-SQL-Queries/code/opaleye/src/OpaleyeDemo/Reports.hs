{-# LANGUAGE Arrows                #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module OpaleyeDemo.Reports where

import           Control.Arrow              (returnA)
import           Data.Int                   (Int64)
import           Data.Profunctor.Product    (p2)
import           Data.Time.Calendar         (Day)
import           Data.Time.Clock            (getCurrentTime, utctDay)
import           Database.PostgreSQL.Simple (Connection)
import           Opaleye                    hiding (not, null)
import           OpaleyeDemo.Flags
import qualified OpaleyeDemo.Hashtag        as H
import qualified OpaleyeDemo.Ids            as I
import qualified OpaleyeDemo.Todo           as T
import qualified OpaleyeDemo.Utils          as U


todosAndHashtags :: Query (T.TodoColumns, H.HashtagNullableColumns)
todosAndHashtags = leftJoin T.todoQuery H.hashtagQuery eqTodoId
    where eqTodoId (todos, hashtags) = T._id todos .=== H._todoId hashtags


todosWithoutHashtags :: Query T.TodoColumns
todosWithoutHashtags = proc () -> do
    (todos, hashtags) <- todosAndHashtags -< ()
    restrict -< isNull ((I.todoId . H._todoId) hashtags)
    returnA -< todos


todosOpDate
    :: (Column PGDate -> Column PGDate -> Column PGBool)
    -> QueryArr Day T.TodoColumns
todosOpDate op = proc day -> do
    todos <- T.todoQuery -< ()
    restrict -< T._dueDate todos `op` pgDay day
    returnA -< todos


lateTodos :: Day -> Query T.TodoColumns
lateTodos day = proc () -> do
    todos <- todosOpDate (.<=) -< day
    returnA -< todos


futureTodos :: Day -> Query T.TodoColumns
futureTodos day = proc () -> do
    todos <- todosOpDate (.>) -< day
    returnA -< todos


countLateTodos :: Day -> Query (Column PGInt8)
countLateTodos day
  = aggregate count . fmap (I.todoId . T._id) $ lateTodos day


countFutureTodos :: Day -> Query (Column PGInt8)
countFutureTodos day
  = aggregate count . fmap (I.todoId . T._id) $ futureTodos day


todoIdsWithHashtagAmount :: Query (Column PGInt4, Column PGInt8)
todoIdsWithHashtagAmount
  = aggregate (p2 (groupBy, count))
  $ proc () -> do
        hashtags <- H.hashtagQuery -< ()
        returnA -< ( (I.todoId . H._todoId) hashtags
                   , (I.hashtagStr . H._hashtag) hashtags)


todosMultipleHashtags :: Query T.TodoColumns
todosMultipleHashtags = proc () -> do
    todos         <- T.todoQuery -< ()
    (tid, hcount) <- todoIdsWithHashtagAmount -< ()
    restrict -< (I.todoId . T._id) todos .== tid
    restrict -< hcount .> pgInt8 1
    returnA -< todos


hashtagsCounted :: Query (Column PGText, Column PGInt8)
hashtagsCounted
  = aggregate (p2 (groupBy, count))
  $ proc () -> do
        hashtags <- H.hashtagQuery -< ()
        returnA -< ( (I.hashtagStr . H._hashtag) hashtags
                   , (I.todoId . H._todoId) hashtags)


mostPopularHashtags :: Query (Column PGText)
mostPopularHashtags = proc () -> do
    (hashtag, hcount) <- hashtagsCounted -< ()
    restrict -< hcount .>= pgInt8 2
    returnA -< hashtag


runReports :: Connection -> [Flag] -> IO ()
runReports conn flags = do
    let printTodo = flip U.todoToString []

    putStrLn "Most popular hashtags:"
    mph <- U.runQ conn mostPopularHashtags flags :: IO [String]
    mapM_ print mph

    putStrLn "Todos without hashtags:"
    twh <- U.runQ conn todosWithoutHashtags flags :: IO [T.Todo]
    mapM_ (putStrLn . printTodo) twh

    now <- fmap utctDay getCurrentTime

    putStrLn "Number of late todos:"
    lt <- U.runQ conn (countLateTodos now) flags :: IO [Int64]
    if null lt then
      print (0 :: Int)
    else
      print . head $ lt

    putStrLn "Number of upcoming todos:"
    ft <- U.runQ conn (countFutureTodos now) flags :: IO [Int64]
    if null ft then
      print (0 :: Int)
    else
      print . head $ ft

    putStrLn "Todos with multiple hashtags:"
    tmh <- U.runQ conn todosMultipleHashtags flags :: IO [T.Todo]
    mapM_ (putStrLn . printTodo) tmh


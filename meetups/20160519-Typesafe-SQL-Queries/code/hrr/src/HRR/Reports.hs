{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HRR.Reports
    ( -- * Exports
      runReports
    , todosAndHashtags
    , todosWithoutHashtags
    , todosMultipleHashtags
    , mostPopularHashtags
    , countLateTodos
    , countFutureTodos
   ) where

import           Data.Int                  (Int32)
import           Data.Time.Calendar        (Day)
import           Data.Time.Clock           (getCurrentTime, utctDay)
import           Database.HDBC             (IConnection)
import           Database.Relational.Query
import qualified HRR.ConnectionHelpers     as C
import qualified HRR.Hashtag               as H
import qualified HRR.Todo                  as T

runReports :: (IConnection conn) => conn -> IO ()
runReports conn = do
    putStrLn "Most popular hashtags:"
    C.run conn () mostPopularHashtags

    putStrLn "Todos without hashtags:"
    C.run conn () todosWithoutHashtags

    now <- fmap utctDay getCurrentTime

    putStrLn "Number of late todos:"
    C.run conn now countLateTodos

    putStrLn "Number of upcoming todos:"
    C.run conn now countFutureTodos

    putStrLn "Todos with multiple hashtags:"
    C.run conn () todosMultipleHashtags

todosAndHashtags :: Relation () (T.Todo, Maybe H.Hashtag)
todosAndHashtags = relation $ do
    t <- query T.todo
    h <- queryMaybe H.hashtag
    on $ just (t ! T.id') .=. h ?! H.todoId'
    return $ t >< h


todosWithoutHashtags :: Relation () T.Todo
todosWithoutHashtags = relation $ do
    t <- query todosAndHashtags
    let todo = t ! fst'
    let maybeHashtag = t ! snd'
    wheres $ isNothing (maybeHashtag ?! H.todoId')
    return $ T.Todo |$| todo ! T.id'
                    |*| todo ! T.title'
                    |*| todo ! T.dueDate'
                    |*| todo ! T.prio'


todosMultipleHashtags :: Relation () Int32
todosMultipleHashtags = aggregateRelation $ do
    t <- query T.todo
    h <- query H.hashtag
    on $ t ! T.id' .=. h ! H.todoId'
    g <- groupBy $ t ! T.id'
    having $ count (h ! H.hashtagStr') .>. value (1 :: Int)
    return g


countLateTodos :: Relation Day Int
countLateTodos = aggregateRelation' . placeholder $ \ph -> do
    t <- query T.todo
    wheres $ t ! T.dueDate' .<=. ph
    return $ count (t ! T.id')


countFutureTodos :: Relation Day Int
countFutureTodos = aggregateRelation' . placeholder $ \ph -> do
    t <- query T.todo
    wheres $ t ! T.dueDate' .>. ph
    return $ count (t ! T.id')


mostPopularHashtags :: Relation () (String, Int32)
mostPopularHashtags = aggregateRelation $ do
    h <- query H.hashtag
    g <- groupBy $ h ! H.hashtagStr'
    having $ count (h ! H.hashtagStr') .>. value (1 :: Int)
    return $ g >< count (h ! H.hashtagStr')


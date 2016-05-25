{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module OpaleyeDemo.Commands
    ( -- * Exports
      runAndPrintCommand
      -- * Commands
    , Command(..)
     -- * Flags
    , Flag(..)
    ) where

import qualified Data.ByteString.Char8                  as B (pack, split,
                                                              unpack)
import           Data.Maybe                             (fromJust, isNothing)
import           Data.Time.Calendar                     (Day, fromGregorian)
import           Database.PostgreSQL.Simple             (Connection)
import           Database.PostgreSQL.Simple.Transaction (withTransaction)
import           Opaleye
import           OpaleyeDemo.Flags
import qualified OpaleyeDemo.Hashtag                    as H
import qualified OpaleyeDemo.Ids                        as I
import qualified OpaleyeDemo.Reports                    as R
import qualified OpaleyeDemo.Todo                       as T
import qualified OpaleyeDemo.Utils                      as U
import           Prelude                                hiding (null)
import           System.Exit
import           System.IO

--------------------------------------------------------------------------------
-- | Main command runner
runAndPrintCommand :: Connection -> Command -> [Flag] -> IO ()
runAndPrintCommand conn (Find x) flags = runFindCommand conn x flags
runAndPrintCommand conn (Add x) flags  = runAddCommand conn x flags
runAndPrintCommand conn (Complete x) _ = runCompleteCommand conn x
runAndPrintCommand conn List flags     = runListCommand conn flags
runAndPrintCommand conn Report flags   = R.runReports conn flags

runFindCommand :: Connection -> Int -> [Flag] -> IO ()
runFindCommand conn x flags = do
    let tid = I.TodoId { I.todoId = x }
    todos <- runQuery conn (T.selectTodo tid) :: IO [T.Todo]

    if length todos == 1 then
      printTodo conn flags (head todos)
    else
      hPutStrLn stderr "Todo not found!" >> exitWith (ExitFailure 1)

runAddCommand :: Connection -> String -> [Flag] -> IO ()
runAddCommand conn title flags = do
    let p       = prioFromFlags flags
    let prio    = if isNothing p then
                    null
                  else
                    toNullable ((pgInt4 . fromJust) p)
    let dueDate = dueDateFromFlags flags
    let todo    = T.Todo { T._id      = I.TodoId { I.todoId = Nothing }
                         , T._title   = pgString title
                         , T._dueDate = pgDay dueDate
                         , T._prio    = I.Prio { I.prio = prio }
                         } :: T.TodoInsertColumns

    tid <- T.insertTodo conn todo
    putStrLn ("Added todo with id " ++ (show . I.todoId) tid)

runCompleteCommand :: Connection -> Int -> IO ()
runCompleteCommand conn i = withTransaction conn $ do
    let tid = I.TodoId { I.todoId = i }
    todos    <- runQuery conn (T.selectTodo tid) :: IO [T.Todo]
    affected <- T.deleteTodo conn tid

    if affected > 0 then
      putStrLn ("Completed todo '" ++ T._title (head todos) ++ "'")
    else
      hPutStrLn stderr "Something went wrong!"

runListCommand :: Connection -> [Flag] -> IO ()
runListCommand conn flags = do
    let todoQuery = if OrderByPriority `notElem` flags then
                      T.todoQuery
                    else
                      T.todosByPriority

    todos <- runQuery conn todoQuery :: IO [T.Todo]
    mapM_ (printTodo conn flags) todos

printTodo :: Connection -> [Flag] -> T.Todo -> IO ()
printTodo conn flags todo = do
    hashtags <- if WithHashtags `elem` flags then
                  runQuery conn (H.hashtagsForTodo (T._id todo)) :: IO [H.Hashtag]
                else
                  return []

    if RawPrint `elem` flags then do
      mapM_ print hashtags
      print todo
    else
      putStrLn $ U.todoToString todo hashtags

--------------------------------------------------------------------------------
-- | Helper function to get the due date from the list of flags
-- | I'm assumning the format is correct
dueDateFromFlags :: [Flag] -> Day
dueDateFromFlags []          = error "Must specify due date!"
dueDateFromFlags (DueBy s:_) = fromGregorian year month day
                                 where
                                   ymd   = B.split '-' (B.pack s)
                                   year  = read (B.unpack (head ymd)) :: Integer
                                   month = read (B.unpack (ymd !! 1)) :: Int
                                   day   = read (B.unpack (ymd !! 2)) :: Int
dueDateFromFlags (_:xs)      = dueDateFromFlags xs

--------------------------------------------------------------------------------
-- | Helper function to get the priority setting from the list of flags
prioFromFlags :: [Flag] -> Maybe Int
prioFromFlags []                = Nothing
prioFromFlags (SetPriority p:_) = Just (read p :: Int)
prioFromFlags (_:xs)            = prioFromFlags xs


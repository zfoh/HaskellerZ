{-# LANGUAGE FlexibleContexts #-}

module HRR.Commands
    ( -- * Exports
      runAndPrintCommand
    , run
    , runDebug
      -- * Commands
    , Command(..)
     -- * Flags
    , Flag(..)
     -- * Export to avoid 'unused' warnings
    , insertTodo
    , runAddCommand
    ) where

import qualified Data.ByteString.Char8     as B (pack, split, unpack)
import           Data.Int                  (Int32)
import           Data.List                 (intercalate)
import           Data.Time.Calendar        (Day, fromGregorian)
import           Database.HDBC             (IConnection, commit)
import           Database.HDBC.Record      (runDelete, runInsert,
                                            runInsertQuery, runQuery)
import           Database.Relational.Query
import           HRR.ConnectionHelpers
import qualified HRR.Hashtag               as H
import qualified HRR.Reports               as R
import qualified HRR.Todo                  as T

--------------------------------------------------------------------------------
-- | Commands available for the application
data Command
    = List                  -- list
    | Find Int32            -- find
    | Add String            -- add
    | Complete Int32        -- complete
    | Reports               -- reports
    deriving (Show)

--------------------------------------------------------------------------------
-- | Flags available for the app
data Flag
    = DueBy String          --due-by
    | Late                  --late
    | WithHashtags          --with-hashtags
    | SearchHashtag String  --hashtags
    | OrderByPriority       --order-by-priority
    | SetPriority String    --priority
    | Debug                 --debug
    | Help                  --help
    | Version               --version
    deriving (Show, Eq)

--------------------------------------------------------------------------------
-- | Main command runner
runAndPrintCommand :: (IConnection conn) => conn -> Command -> [Flag] -> IO ()
runAndPrintCommand conn cmd flags
  = case cmd of
      List       -> runListCommand conn flags
      Find x     -> runFindCommand conn x flags
      Add title  -> runAlternativeAddCommand conn title flags
      Complete x -> runCompleteCommand conn x
      Reports    -> runReportsCommand conn

--------------------------------------------------------------------------------
-- | Reports command
runReportsCommand :: (IConnection conn) => conn -> IO ()
runReportsCommand = R.runReports

--------------------------------------------------------------------------------
-- | List command
runListCommand :: (IConnection conn) => conn -> [Flag] -> IO ()
runListCommand conn flags = do
    let q = relationalQuery $ if OrderByPriority `notElem` flags then
                T.todo
            else
                T.todosByPriority

    if Debug `elem` flags then
        runQDebugPrint conn () q (printTodoList conn flags)
    else
        runQPrint conn () q (printTodoList conn flags)

--------------------------------------------------------------------------------
-- | Add command
insertTodo :: InsertQuery T.PiTodo -- An insert query to be bound by a partial
insertTodo = derivedInsertQuery T.piTodo' . relation' $
    placeholder $ \ph ->
        return $ T.PiTodo |$| ph ! T.piTodoTitle'
                          |*| ph ! T.piTodoDate'
                          |*| ph ! T.piTodoPrio'

runAddCommand :: (IConnection conn) => conn -> String -> [Flag] -> IO ()
runAddCommand conn title flags = do
    let prio   = prioFromFlags flags
    let dueDate = dueDateFromFlags flags

    -- I can build now my partial object
    let piToInsert = T.PiTodo { T.piTodoTitle = title
                              , T.piTodoDate  = dueDate
                              , T.piTodoPrio  = prio
                              }

    i <- runInsertQuery conn insertTodo piToInsert
    -- FIXME: What about RETURNING id?
    commit conn
    print i

--------------------------------------------------------------------------------
-- | Alternative insert command avoiding the use of hrr "applicatives",
-- | avoids creating partial datas for inserts, and is useful for updates
-- | with 'derivedUpdate' function
-- |
-- | In the examples, derivedInsert is always used with the 'value' function
-- | but it's normally a better idea to let the database drivers do the
-- | binding when data comes from the "outside"
insertTodoAlt :: Insert ((String, Day), Maybe Int32)
insertTodoAlt = derivedInsertValue $ do
    (phTitle, ())   <- placeholder (\ph -> T.title'   <-# ph)
    (phDueDate, ()) <- placeholder (\ph -> T.dueDate' <-# ph)
    (phPrio, ())    <- placeholder (\ph -> T.prio'    <-# ph)
    return (phTitle >< phDueDate >< phPrio)

runAlternativeAddCommand :: (IConnection conn)
                         => conn -> String -> [Flag] -> IO ()
runAlternativeAddCommand conn title flags = do
    let dueDate  = dueDateFromFlags flags
    let priority = prioFromFlags flags
    i <- runInsert conn insertTodoAlt ((title, dueDate), priority)
    commit conn
    print i

--------------------------------------------------------------------------------
-- | Find a todo
runFindCommand :: (IConnection conn) => conn -> Int32 -> [Flag] -> IO ()
runFindCommand conn x flags =
    if Debug `elem` flags then
        runQDebugPrint conn x T.selectTodo (printTodo conn)
    else
        runQPrint conn x T.selectTodo (printTodo conn)

printTodo :: (IConnection conn) => conn -> T.Todo -> IO ()
printTodo conn t = do
    hashtags <- let tid = T.id t in
                runQuery conn (relationalQuery findHashtagsForTodo) tid

    putStrLn $ intercalate "\n"
        [
          "Title:    " ++ T.title t
        , "Due by:   " ++ show (T.dueDate t)
        , "Priority: " ++ maybe "-" show (T.prio t)
        , "Hashtags: " ++ intercalate ", " (map H.hashtagStr hashtags)
        ]

printTodoList :: (IConnection conn) => conn -> [Flag] -> T.Todo -> IO ()
printTodoList conn flags t = do
    let withH = WithHashtags `elem` flags
    hashtags <- if withH then
                    let tid = T.id t in
                    runQuery conn (relationalQuery findHashtagsForTodo) tid
                else
                    return []

    if withH then
        putStrLn $ unwords
            [
              show (T.id t) ++ "."
            , T.title t
            , "(due by: "  ++ show (T.dueDate t) ++ ")"
            , "(priority: " ++ maybe "-" show (T.prio t) ++ ")"
            , intercalate ", " (map H.hashtagStr hashtags)
            ]
    else
         putStrLn $ unwords
            [
              show (T.id t) ++ "."
            , T.title t
            , "(due by: "  ++ show (T.dueDate t) ++ ")"
            , "(priority: " ++ maybe "-" show (T.prio t) ++ ")"
            ]

findHashtagsForTodo :: Relation Int32 H.Hashtag
findHashtagsForTodo = relation' . placeholder $ \ph -> do
    hashtags <- query H.hashtag
    wheres $ hashtags ! H.todoId' .=. ph
    return hashtags

--------------------------------------------------------------------------------
-- | Delete a todo
deleteTodo :: Delete Int32
deleteTodo = derivedDelete $ \projection ->
                fst <$> placeholder
                    (\ph -> wheres $ projection ! T.id' .=. ph)

runCompleteCommand :: (IConnection conn) => conn -> Int32 -> IO ()
runCompleteCommand conn x = do
    todos <- runQuery conn T.selectTodo x
    i     <- runDelete conn deleteTodo x

    if i > 0 then
        putStrLn ("Compeleted todo " ++ T.title (head todos))
    else
        putStrLn ("Could not complete todo " ++ show x)

    commit conn

--------------------------------------------------------------------------------
-- | Helper function to get the due date from the list of flags
-- | again please specify proper format, there's no real checking :)
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
prioFromFlags :: [Flag] -> Maybe Int32
prioFromFlags []                = Nothing
prioFromFlags (SetPriority p:_) = Just (read p :: Int32)
prioFromFlags (_:xs)            = prioFromFlags xs


{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HRR.ConnectionHelpers
      ( -- * Exports
        runDebug
      , run
      , runQDebug
      , runQ
      , runQDebugPrint
      , runQPrint
      ) where

import           Database.HDBC             (IConnection, SqlValue)
import           Database.HDBC.Record      (runQuery)
import           Database.Record           (FromSql, ToSql)
import           Database.Relational.Query (Query, Relation, relationalQuery)

--------------------------------------------------------------------------------
-- | Run a relation to the connection but first log the query to stdout
runDebug :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
         => conn -> p -> Relation p a -> IO ()
runDebug conn param rel = runQDebug conn param (relationalQuery rel)

--------------------------------------------------------------------------------
-- | Run a relation to the connection
run :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
    => conn -> p -> Relation p a -> IO ()
run conn param rel = runQ conn param (relationalQuery rel)

--------------------------------------------------------------------------------
-- | Run a relation to the connection but first log the query to stdout
runQDebug :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
          => conn -> p -> Query p a -> IO ()
runQDebug conn param rel = do
  putStrLn $ "sql debug: " ++ show rel
  records <- runQuery conn rel param
  mapM_ print records

--------------------------------------------------------------------------------
-- | Run a relation to the connection
runQ :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
    => conn -> p -> Query p a -> IO ()
runQ conn param rel = do
  records <- runQuery conn rel param
  mapM_ print records

--------------------------------------------------------------------------------
-- | Run a relation to the connection but first log the query to stdout
-- | Then print the records with some provided print function
runQDebugPrint :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
               => conn -> p -> Query p a -> (a -> IO ()) -> IO ()
runQDebugPrint conn param rel print' = do
  putStrLn $ "sql debug: " ++ show rel
  records <- runQuery conn rel param
  mapM_ print' records

--------------------------------------------------------------------------------
-- | Run a relation to the connection with some provided print function
runQPrint :: (Show a, IConnection conn, FromSql SqlValue a, ToSql SqlValue p)
          => conn -> p -> Query p a -> (a -> IO ()) ->IO ()
runQPrint conn param rel print' = do
  records <- runQuery conn rel param
  mapM_ print' records


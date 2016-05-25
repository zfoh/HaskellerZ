module HRR.DataSource
     ( -- Exports
       defineTable
     , connect'
     ) where

import           Database.HDBC.PostgreSQL        (Connection, connectPostgreSQL)
import           Database.HDBC.Query.TH          (defineTableFromDB)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           Language.Haskell.TH

-- | The string is a connection string is based on
-- | http://www.postgresql.org/docs/9.3/static/libpq-connect.html#LIBPQ-CONNSTRING
-- | More info: http://www.ietf.org/rfc/rfc3986.txt
connect' :: IO Connection
connect' = connectPostgreSQL "dbname=postgres"

-- | Defines a table and generates the data
-- | from the table definition
-- | By using TemplateHaskell, this means that there will need to be
-- | a database connection when compiling the program
-- | Types are converted by default according to this list
-- | https://github.com/khibino/haskell-relational-record/blob/master/relational-schemas/src/Database/Relational/Schema/PostgreSQL.hs#L59
defineTable :: String -> -- ^ Schema name
               String -> -- ^ Table name
               [Name] -> -- ^ Derives
               Q [Dec]   -- ^ Quoted result
defineTable = defineTableFromDB connect' driverPostgreSQL


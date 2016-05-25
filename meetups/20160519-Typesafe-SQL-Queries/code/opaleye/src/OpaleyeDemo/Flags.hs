module OpaleyeDemo.Flags where

--------------------------------------------------------------------------------
-- | Commands available for the application
data Command
    = List                  -- ^ "list" command
    | Find Int              -- ^ "find" command
    | Add String            -- ^ "add" command
    | Complete Int          -- ^ "complete" command
    | Report                -- ^ "report" command
    deriving (Show)

--------------------------------------------------------------------------------
-- | Flags available for the app
data Flag
    = DueBy String          -- ^ --due-by
    | Late                  -- ^ --late
    | WithHashtags          -- ^ --with-hashtags
    | SearchHashtag String  -- ^ --hashtags
    | OrderByPriority       -- ^ --order-by-priority
    | SetPriority String    -- ^ --priority
    | Help                  -- ^ --help
    | Version               -- ^ --version
    | RawPrint              -- ^ --raw
    | Debug                 -- ^ --debug
    deriving (Show, Eq)



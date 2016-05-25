{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Database.PostgreSQL.Simple
import           Simple.Commands
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO

--------------------------------------------------------------------------------
-- | Flags with their corresponding description
flags :: [OptDescr Flag]
flags = [ Option ['d'] ["due-by"]              (ReqArg DueBy "DATE")
               "Get todos due by a certain date"
        , Option ['l'] ["late"]                (NoArg Late)
                "Get todos that are late"
        , Option ['w'] ["with-hashtags"]       (NoArg WithHashtags)
                "Get todos with the associated hashtags"
        , Option ['h'] ["hashtags"]            (ReqArg SearchHashtag "HASHTAGS")
                "Get todos for a certain hashtag"
        , Option ['o'] ["order-by-priority"]   (NoArg OrderByPriority)
                "Get todos ordered by priority"
        , Option ['p'] ["priority"]            (ReqArg SetPriority "PRIORITY")
                "When adding a todo, you can set its priority"
        , Option ['h'] ["help"]                (NoArg Help)
                "Show help menu"
        , Option ['v'] ["version"]             (NoArg Version)
                "Show version"
        ]

header :: String
header = "todos <command> [options]\n\n\
         \Available commands: find, add, complete, list\n"

--------------------------------------------------------------------------------
-- | Parse the command and the corresponding flags (if any)
parse :: [String] -> Either String (Command, [Flag])
-- | Simple parsing of commands
parse []                = Left "Wrong number of arguments."
parse ("-h":_)          = Left (usageInfo header flags)
parse ("--help":_)      = Left (usageInfo header flags)
parse ("-v":_)          = Left "0.1.0.0"
parse ("--version":_)   = Left "0.1.0.0"
parse args = case args of
               ("find":x:argv)  -> makeCommand (Find (read x :: Int)) argv
               ("add":x:argv)   -> makeCommand (Add x) argv
               ("complete":x:_) -> makeCommand (Complete (read x :: Int)) []
               ("list":argv)    -> makeCommand List argv
               _                -> Left "Unrecognized command or wrong \
                                        \number of arguments."

makeCommand :: Command -> [String] -> Either String (Command, [Flag])
makeCommand c argv = case getOpt Permute flags argv of
                       (args', _, []) -> Right(c, args')
                       (_, _, errs)   -> Left (concat errs)

--------------------------------------------------------------------------------
-- | Gets the results of a command and its corresponding flags
getResults :: Connection -> Either String (Command, [Flag]) -> IO ()
-- | An error happened
getResults _ (Left s) = hPutStrLn stderr s
                     >> exitWith (ExitFailure 1)
-- | We have an appropriate command and its results
getResults conn (Right (c, flags')) = runAndPrintCommand conn c flags'
                                   >> exitSuccess

main :: IO ()
main = do
    parsed <- fmap parse getArgs
    conn   <- connect defaultConnectInfo
    getResults conn parsed

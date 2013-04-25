#!/usr/bin/env runhaskell

import System.Console.GetOpt
import System.Environment

data MyOptions =
  MyOptions { name :: String
            , times :: Int }

def = MyOptions { name = "Indiana Jones", times = 4 }

options =
  [ Option ['n'] ["name"]
    (ReqArg (\name r -> r { name = name }) "NAME") "Who to greet"
  , Option ['t'] ["times"]
    (ReqArg (\times r -> r { times = read times}) "INT") "How many times" ]

run opts = sequence_ $ replicate (times opts) $ putStrLn (name opts)

main = do
  argv <- getArgs
  case getOpt Permute options argv of
    (o,n,[]  ) -> run (foldr ($) def o)
    (_,_,errs) -> ioError (userError $ concat errs ++ usageInfo "example" options)

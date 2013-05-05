#!/usr/bin/env runhaskell

import Control.Applicative
import System.Console.CmdTheLine

name :: Term String
name = value $ opt "Indiana Jones" $ optInfo [ "name", "n" ]

times :: Term Int
times = value $ opt 4 $ optInfo [ "times", "t" ]

hello :: String -> Int -> IO ()
hello name times = sequence_ $ replicate times $ putStrLn name

term :: Term (IO ())
term = hello <$> name <*> times

termInfo :: TermInfo
termInfo = defTI { termName = "Hello", version = "1.0" }

main :: IO ()
main = run ( term, termInfo )

{-

import Control.Monad




-- getLine :: IO String

-- putStrLn



cat1 :: IO ()
cat1 = do
  x <- getLine
  putStrLn x
  cat1


cat2 :: IO ()
cat2 = do
  eof <- hIsEOF stdin
  unless eof $ do
    l <- hGetLine stdin
    putStrLn l
    cat2

stream-of-chars-terminated
~>
stream of lines
~>
sink-into-stdout

-}

import Conduit
import System.IO


cat3 :: IO ()
cat3 = runConduitRes $ stdinC .| linesUnboundedAsciiC .| stdoutC

{-
main :: IO ()
main = do
    -- Pure operations: summing numbers.
    print $ runConduitPure $ yieldMany [1..10] .| sumC

    -- Exception safe file access: copy a file.
    writeFile "input.txt" "This is a test." -- create the source file
    runConduitRes $ sourceFileBS "input.txt" .| sinkFile "output.txt" -- actual copying
    readFile "output.txt" >>= putStrLn -- prove that it worked

    -- Perform transformations.
    print $ runConduitPure $ yieldMany [1..10] .| mapC (+ 1) .| sinkList
-}

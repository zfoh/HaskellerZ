-- | A simple clone of the unix `wc` utility.
--
-- Note learn about Haddock syntax early on. Then you can just use it.
module Main (main) where

-- Note that name resolution is key to understanding code! So make the life of
-- your code reviewer (your future self) as easy as possible by maintaining
-- your imports well.
--
-- I group and sort imports alphabetically. I also use some horizontal
-- whitespace to improve readability. This has worked well in many projects.

import           Control.Monad                (forM_)

import qualified Data.ByteString              as B
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE

import           System.Directory             (doesFileExist)
import           System.Environment           (getArgs)


main :: IO ()
main = do
    -- Poor-man's argument parsing. Use 'optparse-applicative' for proper
    -- command line parsing, with help message etc.
    args0 <- getArgs
    case args0 of
      []            -> putStrLn "wc: no arguments given"
      ("-c" : args) -> use (B.length)                         args
      ("-m" : args) -> use (T.length . TE.decodeUtf8)         args
      ("-l" : args) -> use (length . T.lines . TE.decodeUtf8) args
      ("-w" : args) -> use (length . T.words . TE.decodeUtf8) args
      (flag : _   ) -> putStrLn $ "wc: unsupported flag '" ++ flag ++ "'."

-- | Summarize a number of files using the given function.
--
-- (This is a ♥ to have a multi-byte character in our source code.)
use :: Show a => (B.ByteString -> a) -> [FilePath] -> IO ()
use summarizeBytes fileNames =
    forM_ fileNames $ \fileName -> do
        isFile  <- doesFileExist fileName
        summary <-
          if not isFile
            then return "not a file"
            else do
              --
              -- Note that we are reading the whole file content into memory. This
              -- is definitely inefficient and unnecessary. For smallish files this
              -- is however totally OK, and it allows us to demonstrate the
              -- composition of a Haskell applications from a pure core and an IO
              -- layer.
              --
              -- For proper streaming implementations with the same compositional
              -- structure one would use the `conduit` or `pipes` libraries. Don't
              -- use lazy IO except you really know what you are doing!
              --
              content <- B.readFile fileName
              return (show $ summarizeBytes content)

        putStrLn $ fileName ++ "   " ++ summary

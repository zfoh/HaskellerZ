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

import qualified Data.Text.ByteString         as B
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as TE

import           System.Environment           (getArgs)


main :: IO ()
main = do
    -- Poor man's argument parsing. Use 'optparse-applicative' for proper
    -- command line parsing, with help message etc.
    args0 <- getArgs
    case args0 of
      []            -> putStdErr "wc: no arguments given"
      ("-c" : args) -> use (B.length)
      ("-m" : args) -> use (T.length . TE.decodeUtf8)
      ("-l" : args) -> use (length . T.lines . TE.decodeUtf8)
      ("-w" : args) -> use (length . T.words . TE.decodeUtf8)
      (flag : args) -> putStdErr $ "unsupported flag: " ++ flag


use :: Show a => (B.ByteString -> a) -> [FilePath] -> IO ()
use summarizeBytes fileNames =
    forM_ fileNames $ \fileName -> do
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
        return (f content)

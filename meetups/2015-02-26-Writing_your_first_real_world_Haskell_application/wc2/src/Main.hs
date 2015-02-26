
import           Control.Monad

import qualified Data.ByteString    as B
import           Data.List
import           Data.Maybe
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           Data.Monoid        ((<>))

import           System.Environment (getArgs)


main :: IO ()
main = do
    args <- getArgs
    case args of
      []            -> putStrLn "ERROR: no arguments provided."
      (flags:files) -> summarize (mapMaybe flagToSummary flags) files
  where
    flagToSummary :: Char -> Maybe (B.ByteString -> Int)
    flagToSummary c = case c of
        'l' -> Just (length . T.lines . TE.decodeUtf8)
        'w' -> Just (length . T.words . TE.decodeUtf8)
        'c' -> Just (T.length .         TE.decodeUtf8)
        'b' -> Just (B.length)
        _   -> Nothing

    summarize :: Show a => [B.ByteString -> a] -> [FilePath] -> IO ()
    summarize fs files = do
        forM_ files $ \file -> do
            inp <- B.readFile file
            let summaries = [ f inp | f <- fs ]
                ppSummaries = concat $ intersperse ", " $ map show summaries

            putStrLn $ file <> ": " <> ppSummaries





{-|

Tranditional, non-transformer way of using Monads.

Several things to notice here:

1. The types of functions. We are in the IO monad and the results are collected as list of pairs

    IO [(FilePath, Int)]

2. The way results are returned. We construct a list as outcome of recursive dive into the folders.

|-}
module CountEntries () where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

-- monad related imports
import Control.Monad (forM, liftM)

-- | Counts number of entries in folders as it traverses the folder tree.
--   Stores the results in a list.
countEntries :: FilePath -> IO [(FilePath, Int)]
countEntries path = do
  contents <- listDirectory path
  rest <- forM contents $ \name -> do
    let newName = path </> name
    isDir <- doesDirectoryExist newName
    if isDir
      then countEntries newName
      else return []
  return $ (path, length contents) : concat rest

-- | Get directory listing without the "." and ".." references
listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

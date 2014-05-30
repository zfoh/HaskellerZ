{-|

Using the WriterT transformer, from the transformers package to write the entry count
into the "log" along the way as computation visits the folder tree.

Things to notice here:

1. Types of the functions.

   WriterT [(FilePath, Int)] IO ()

2. We are in the WriterT monad and results are stored in the writer log.
3. IO monad is wrapped by the WriterT type
4. We had to use liftIO function to run the IO operation
5. The result is not returned at the end of computation by constructing a list.

|-}
module CountEntriesWriter () where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

-- monad related imports
import Control.Monad (forM_, when, liftM)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer (WriterT, runWriterT, tell)

-- | Traverses the folder tree and counts number of entries.
--   Writes results into a writer log along the way.
countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
    contents <- liftIO . listDirectory $ path
    -- store the value in the log
    tell [(path, length contents)]
    forM_ contents $ \name -> do
      let newName = path </> name
      isDir <- liftIO . doesDirectoryExist $ newName
      when isDir $ countEntries newName

-- | Get directory listing without the "." and ".." references
listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

{-|

Reordering the monad stack and showing the effect on the "effect basis" functions

Things to notice here:

1. Reordered monad stack:

   WriterT CounterLog (StateT CounterState (ReaderT CounterConfig IO)) a

2. Updated "effect basis" functions.

|-}
module CountEntriesStack () where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

-- monad related imports
import Control.Monad (forM_, when, liftM)
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Writer
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State


-- | Configuration of the computation, used as ReaderT monad value
data CounterConfig = CounterConfig {
    maxDepth :: Int
  } deriving (Show)

-- | Current state of the computation, used for the StateT monad value
data CounterState = CounterState {
    currentDepth :: Int
  } deriving (Show)

-- | Type renamer for the WriterT log values.
type CounterLog = [(FilePath, Int)]

-- | Our monad stack.
type Counter a = WriterT CounterLog (StateT CounterState (ReaderT CounterConfig IO)) a

-- | Runner of the monad with provided configuration.
runCounter :: Counter a -> CounterConfig -> IO ((a, CounterLog), CounterState)
runCounter m config =
  let initialState = CounterState 0
  in
   runReaderT (runStateT (runWriterT m) initialState) config

-- | Lift writer operation.
counterTell :: CounterLog -> Counter ()
counterTell = tell

-- | Lift state get operation.
counterGet :: Counter CounterState
counterGet = lift get

-- | lift state put operation.
counterPut :: CounterState -> Counter ()
counterPut = lift . put

counterAsk :: Counter CounterConfig
counterAsk = (lift . lift) ask

-- | Traverses the folder tree and counts number of entries.
--   Writes results into a writer log along the way.
countEntries :: FilePath -> Counter ()
countEntries path = do
    contents <- liftIO . listDirectory $ path
    -- gets the configuration
    cfg <- counterAsk
    -- gets the current state
    st <- counterGet
    -- records the log
    counterTell [(path, length contents)]
    forM_ contents $ \name -> do
      let newPath = path </> name
          depth = currentDepth st
      isDir <- liftIO . doesDirectoryExist $ newPath
      when (isDir && depth < maxDepth cfg) $ do
        -- updates the state
        counterPut st {currentDepth = depth + 1}
        countEntries newPath

-- | Get directory listing without the "." and ".." references
listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
    where notDots p = p /= "." && p /= ".."

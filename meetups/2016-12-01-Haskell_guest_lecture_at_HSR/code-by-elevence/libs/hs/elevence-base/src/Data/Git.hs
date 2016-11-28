-- | Git utilities, which haven't found a better place in our codebase yet.
module Data.Git
  ( findGitRoot
  ) where

import System.Directory
import System.FilePath.Posix

-- | Returns a path to the most immediate git repository walking
-- upwards from the current directory.
findGitRoot :: IO (Maybe FilePath)
findGitRoot = do
    getCurrentDirectory >>= checkDir 128
  where checkDir :: Int -> FilePath -> IO (Maybe FilePath)
        checkDir 0 _  = return Nothing
        checkDir n wd = do
            let filepath = wd </> ".git"
            e <- isRepo filepath
            if e
              then return $ Just wd
              else checkDir (n - 1) $ takeDirectory wd
        isRepo :: FilePath -> IO Bool
        isRepo path = do
            dir     <- doesDirectoryExist path
            subDirs <- mapM (doesDirectoryExist . (path </>))
                         [ "hooks", "info"
                         , "objects", "refs"
                         , "refs"</> "heads", "refs"</> "tags"]
            return $ and ([dir] ++ subDirs)

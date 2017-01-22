{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Dir
  ( createDir
  , openDir
  , readDir
  ) where

import           System.Fuse
import           System.Posix.Files
import           System.Posix.Types

import           DB.Find
import           DB.Model
import           Debug                   (dbg)
import           Stat                    (dirStat)



{- | https://www.cs.hmc.edu/~geoff/classes/hmc.cs135.201001/homework/fuse/fuse_doc.html

readdir(const char* path, void* buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info* fi)

Return one or more directory entries (struct dirent) to the
caller. … [Lots of stuff elided.] …

It's also important to note that readdir can return errors in a number
of instances; in particular it can return -EBADF if the file handle is
invalid, or -ENOENT if you use the path argument and the path doesn't
exist.

-}

{- | Entire contents of dir.
-}
readDir ∷ DB → FilePath → IO (Either Errno [(FilePath, FileStat)])
readDir db filePath = do
  dbg $ "ReadDir: " ++ filePath

  ctx ← getFuseContext
  return $ Right [ (".",  dirStat ctx)
                 , ("..", dirStat ctx)
                 ]
  -- filesAndStats ← getFilesAndStats db filePath
  -- return $ case filesAndStats of
  --            [] → Left eNOENT
  --            _  → Right filesAndStats


-- TODO
getFilesAndStats ∷ DB → FilePath → IO [(FilePath, FileStat)]
getFilesAndStats db filePath = undefined



-- TODO
{- | What's in FileName? So we know how to use it to create tag(s).
-}
createNewTags ∷ DB → FileName → FileStat → IO ()
createNewTags db fileName fStat = undefined


{- | Create new tags (as necessary) for dirs in path.
-}
createDir ∷ DB → FilePath → FileMode → IO Errno
createDir db filePath mode = do
  dbg $ "creating directory with path: " ++ filePath

  ctx ← getFuseContext
  let (_:fileName) = filePath
  let newStat = (dirStat ctx) { statFileMode = mode }
  createNewTags db fileName newStat
  return eOK



--------------------

{- | If filePath maps to a dir: eOK. Else: eNOENT.
-}
openDir ∷ DB → FilePath → IO Errno
openDir db filePath = do
  dbg $ "Opening dir " ++ filePath
  if filePath == "/"
    then return eOK
    else do let (_:fileName) = filePath
            -- FIXME: Logic is too simple.
            -- Check whether any files have this combo of tags.
            ex ← tagExists db fileName
            if ex
              then return eOK
              else return eNOENT


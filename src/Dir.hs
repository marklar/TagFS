{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Dir
  ( createDir
  , openDir
  , readDir
  , removeDir
  ) where

import           System.Fuse
import           System.Posix.Types

import           DB.Find
import           DB.Insert               (rmFileTag)   -- FIXME
import           DB.Model
import           Debug                   (dbg)
import           Parse                   (parseDirPath)
import           Stat                    (dirStat, fileStat)


{- | What should this mean?

E.g.
$ rmdir /foo/bar
Delete tag 'bar' (but not 'foo')?
Delete tags 'foo' and 'bar' both?

Answer: Rm FileTag 'bar' from files w/ both tags.
-}

-- TODO
removeDir ∷ DB → FilePath → IO Errno
removeDir db filePath = do
  dbg $ "removeDir: " ++ filePath
  -- Split filePath into [Tag].
  let tagNames = parseDirPath filePath
  -- Find all files (if any) with that (complete) tagSet.
  fileEntities ← fileEntitiesFromTags db tagNames
  if null fileEntities
    then return eNOENT
    else do let tagName = last tagNames
            -- Untag last tag from each.
            mapM_ (\id → rmFileTag db id tagName)
              (map (\(FileEntity id _) → id) fileEntities)
            return eOK


{- | https://www.cs.hmc.edu/~geoff/classes/hmc.cs135.201001/homework/fuse/fuse_doc.html

readdir(const char* path, void* buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info* fi)

Return one or more directory entries (struct dirent) to the caller. …
[Lots of stuff elided.] …

Possible error: eNOENT if the path doesn't exist.
-}

{- | Entire contents of dir. Not just files - also sub-dirs (tags).
-}
readDir ∷ DB → FilePath → IO (Either Errno [(FilePath, FileStat)])
readDir db filePath = do
  dbg $ "ReadDir: " ++ filePath

  ctx ← getFuseContext
  let baseDirs = [ (".",  dirStat ctx)
                 , ("..", dirStat ctx)
                 ]

  if filePath == "/"
    -- Include ALL TAGS and ALL FILES.
    then do fileEntities ← allFileEntities db
            r ← namesWithStats db fileEntities
            return $ Right (baseDirs ++ r)

    -- Find all files (if any) with that (complete) tagSet.
    else do fileEntities ← fileEntitiesFromTags db (parseDirPath filePath)
            dbg $ "  num files: " ++ (show $ length fileEntities)
            if null fileEntities
              then return $ Left eNOENT
              else do r ← namesWithStats db fileEntities
                      return $ Right (baseDirs ++ r)


namesWithStats ∷ DB → [Entity] → IO [(FileName, FileStat)]
namesWithStats db fileEntities = do
  ctx ← getFuseContext
  return $ flip map fileEntities (\(FileEntity _ (File name _)) →
                                    (name, fileStat ctx))


-- TODO
{- | What's in FileName? So we know how to use it to create tag(s).
-}
createNewTags ∷ DB → FileName → FileStat → IO ()
createNewTags db fileName fStat = undefined


{- | Create new tags (as necessary) for dirs in path.
-}
createDir ∷ DB → FilePath → FileMode → IO Errno
createDir db filePath mode = do
  dbg $ "createDir w/ path: " ++ filePath

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
    else do let tagNames = parseDirPath filePath
            fileEntities ← fileEntitiesFromTags db tagNames
            if null fileEntities
              then return eNOENT
              else do dbg "  Found dir"
                      return eOK

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Dir
  ( createDir
  , openDir
  , readDir
  , removeDir
  ) where

import           Data.List               (union, (\\))
import           Data.Maybe              (catMaybes)
import           System.Fuse
import           System.Posix.Types

import           DB.Find
import           DB.Insert               (rmFileTag)   -- FIXME
import           DB.Model
import           Debug                   (dbg)
import           Parse                   (parseDirPath)
import           Stat                    (dirStat, fileStat)


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
            files ← fileNamesWithStats db fileEntities
            tagEntities ← allTagEntities db
            tags ← tagNamesWithStats db tagEntities
            return $ Right (baseDirs ++ tags ++ files)

    -- Find all files (if any) with that (complete) tagSet.
    else do let tagNames = parseDirPath filePath
            fileEntities ← fileEntitiesFromTags db tagNames
            dbg $ "  num files: " ++ (show $ length fileEntities)
            if null fileEntities
              then return $ Left eNOENT
              else do files ← fileNamesWithStats db fileEntities
                      tagEntities ← allTagsForFilesExcept db fileEntities tagNames
                      -- filter out the ones whose name match here
                      tags ← tagNamesWithStats db tagEntities
                      return $ Right (baseDirs ++ files ++ tags)


allTagsForFilesExcept ∷ DB → [Entity] → [TagName] → IO [Entity]
allTagsForFilesExcept db fileEntities tagNames = do
  let fileNames = map (\(FileEntity _ (File name _)) → name) fileEntities
  tagNameLists ← mapM (tagsForFileName db) fileNames
  let allTagNames = foldr1 union tagNameLists
  maybeEntities ← mapM (tagEntityNamed db) (allTagNames \\ tagNames)
  return $ catMaybes maybeEntities


tagNamesWithStats ∷ DB → [Entity] → IO [(FileName, FileStat)]
tagNamesWithStats db tagEntities = do
  ctx ← getFuseContext
  return $ flip map tagEntities (\(TagEntity _ (Tag name)) →
                                   (name, dirStat ctx))


fileNamesWithStats ∷ DB → [Entity] → IO [(FileName, FileStat)]
fileNamesWithStats db fileEntities = do
  ctx ← getFuseContext
  return $ flip map fileEntities (\(FileEntity _ (File name _)) →
                                    (name, fileStat ctx))


--------------------

{- | If filePath maps to a dir: eOK. Else: eNOENT.
-}
openDir ∷ DB → FilePath → IO Errno
openDir db filePath = do
  dbg $ "Opening dir " ++ filePath
  if filePath == "/"
    then return eOK
    else do fileEntities ← fileEntitiesFromTags db (parseDirPath filePath)
            if null fileEntities
              then return eNOENT
              else do dbg "  Found dir"
                      return eOK


---------------------


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


-- TODO
{- | What's in FileName? So we know how to use it to create tag(s).
-}
createNewTags ∷ DB → FileName → FileStat → IO ()
createNewTags db fileName fStat = undefined


----------------

{- | What should this mean?

E.g.
$ rmdir /foo/bar
Delete tag 'bar' (but not 'foo')?
Delete tags 'foo' and 'bar' both?

Answer: Rm FileTag 'bar' from files w/ both tags.
-}
removeDir ∷ DB → FilePath → IO Errno
removeDir db filePath = do
  dbg $ "RemoveDir: " ++ filePath
  -- Split filePath into [Tag].
  let tagNames = parseDirPath filePath
  -- Find all files (if any) with that (complete) tagSet.
  fileEntities ← fileEntitiesFromTags db tagNames
  if null fileEntities
    then return eNOENT
    else do let tagName = last tagNames
            -- Untag last tag from each.
            mapM_ (\f → rmFileTag db (fileId f) tagName) fileEntities
            return eOK

{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Dir
  ( createDir
  , openDir
  , removeDir
  ) where

import           System.Fuse
import           System.Posix.Types

import           DB.Find
import           DB.Insert               -- (rmFileTag)   -- FIXME
import           DB.Model
import           Debug                   (dbg)
import           Parse                   (parseDirPath)
import           Stat                    (dirStat)



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
  case parseDirPath filePath of
    [] →
      return eNOENT

    tagNames → do
      -- Create a dummy file to inhabit it. (Don't display it.)
      dummyFileId ← findOrCreateDummyFileId db
      -- Create FileTag for each of tagNames.
      -- FIXME: use newDirStat
      tagIds ← mapM (findOrCreateTagId db) tagNames
      mapM_ (mkFileTag db dummyFileId) tagIds
      return eOK


newDirStat ∷ FileMode → IO FileStat
newDirStat mode = do
  ctx ← getFuseContext
  return $ (dirStat ctx) { statFileMode = mode }
  

findOrCreateTagId ∷ DB → TagName → IO TagId
findOrCreateTagId db tagName = do
  maybeTagEntity ← tagEntityNamed db tagName
  case maybeTagEntity of
    Just te →
      return $ tagId te
    Nothing → do
      mkTag db (Tag tagName)
      findOrCreateTagId db tagName


findOrCreateDummyFileId ∷ DB → IO FileId
findOrCreateDummyFileId db = do
  maybeFileEntity ← fileEntityNamed db "dummy"
  case maybeFileEntity of
    Just fe →
      return $ fileId fe
    Nothing → do
      mkFile db (File "dummy" "")
      findOrCreateDummyFileId db


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

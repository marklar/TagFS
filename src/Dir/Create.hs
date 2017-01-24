{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Dir.Create
  ( createDir
  ) where

import           Database.HDBC           (fromSql, toSql)
import           System.Fuse
import           System.Posix.Types      (FileMode)

import           DB.Base
import           DB.Read
import           DB.Write
import           Debug                   (dbg)
import           Parse                   (parseDirPath)
import           Stat.Base               (dirStat)


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
      mapM_ (ensureFileTag db dummyFileId) tagIds
      return eOK


-- ^ find or make
ensureFileTag ∷ DB → FileId → TagId → IO ()
ensureFileTag db fileId tagId = do
  let sql = "SELECT  * " ++
            "FROM    files_tags " ++
            "WHERE   file_id = ? " ++
            "AND     tag_id  = ?"
  rows ← queryWithClone db sql [toSql fileId, toSql tagId]
  if null rows
    then mkFileTag db fileId tagId
    else return ()


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

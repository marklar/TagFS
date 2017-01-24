{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Device where

import qualified Data.ByteString.Char8   as B
import           System.Fuse
import           Data.Maybe              (catMaybes)
import           System.Posix.Files
import           System.Posix.Types

import           Debug                   (dbg)
import           DB.Model
import           DB.Read
import           DB.Write
import           Parse
import           Stat.Base               (dirStat, fileStat)
import           Types


{- | If asked to create a RegularFile, create an empty one w/ provided
   mode & return eOK. If some other type of device: eNOENT.
-}
tCreateDevice ∷ DB
              → FilePath   -- FilePath ~ String
              → EntryType  -- FUSE: The Unix node type in FS (RegularFile | Directory | …)
              → FileMode   -- System.Posix.Types
              → DeviceID   -- System.Posix.Types
              → IO Errno   -- Foreign.C.Error
tCreateDevice db filePath entryType mode deviceId = do
  dbg $ "CreateDevice, path: " ++ filePath ++ ", entryType: " ++ show entryType
  case entryType of
    RegularFile → do
      let (tagNames, maybeFileName) = parseFilePath filePath

      case maybeFileName of
        Nothing →
          return eNOENT

        Just n → do
          fileId ← createNewFile db n mode
          tagFile db fileId tagNames
          return eOK

    _ → do
      dbg $ "Failed to create unknown device type with path: " ++ filePath
      return eNOENT


tagFile ∷ DB → FileId → [TagName] → IO ()
tagFile db fileId tagNames = do
  maybeTagEntities ← mapM (tagEntityNamed db) tagNames
  let tagIds = map tagId (catMaybes maybeTagEntities)
  mapM_ (mkFileTag db fileId) tagIds


createNewFile ∷ DB → FileName → FileMode → IO FileId
createNewFile db name mode = do
  -- TODO: Store stat info w/ file.
  ctx ← getFuseContext
  let newStat = (fileStat ctx) { statFileMode = mode }
  mkFile db (File name B.empty)
  newFileIdFromName db name


newFileIdFromName ∷ DB → FileName → IO FileId
newFileIdFromName db name = do
  maybeFileEntity ← fileEntityNamed db name
  case maybeFileEntity of
    Nothing →
      error "We just created the file; should exist."
    Just (FileEntity fileId (File _ _)) →
      return fileId

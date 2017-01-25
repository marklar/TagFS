{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module File.Create
  ( tCreateDevice
  ) where

import qualified Data.ByteString.Char8   as B
import           System.Fuse
import           Data.Maybe              (catMaybes)
import           System.Posix.Types      (FileMode, DeviceID)
import           System.FilePath.Posix   (splitExtension, addExtension)

import           Debug                   (dbg)
import           DB.Base
import           DB.Read                 (fileEntityNamed)
import           DB.Write
import           File.Util               (tagFile)  -- move to DB.Write?
import           File.Version            (versionedFileName)
import           Parse
import           Stat.Base               (dirStat, fileStat)
import           Types


{- | If asked to create a RegularFile, create an empty one w/ provided
   mode & return eOK. If some other type of device: eNOENT.

   TODO:
   If fileName is already found among existing files, then append
   suffix to end of filename (before extension).
             e.g.:  foo.txt  ⇒  foo(1).txt
-}
tCreateDevice ∷ DB
              → FilePath   -- FilePath ~ String
              → EntryType  -- FUSE: Unix FS node type (RegularFile | Directory | …)
              → FileMode   -- System.Posix.Types
              → DeviceID   -- System.Posix.Types
              → IO Errno   -- Foreign.C.Error
tCreateDevice db filePath entryType mode deviceId = do
  dbg $ "CreateDevice, path: " ++ filePath ++ ", entryType: " ++ show entryType
  case entryType of
    RegularFile →
      case maybeFileName of
        Nothing →
          return eNOENT

        Just n → do
          fileId ← createNewFile db n mode
          tagFile db fileId tagNames
          return eOK
      where
        (tagNames, maybeFileName) = parseFilePath filePath

    _ → do
      dbg $ "Failed to create unknown device type with path: " ++ filePath
      return eNOENT


---------------------


createNewFile ∷ DB → FileName → FileMode → IO FileId
createNewFile db name mode = do
  -- TODO: Store stat info w/ file.
  ctx ← getFuseContext
  let newStat = (fileStat ctx) { statFileMode = mode }
  name' ← versionedFileName db name
  mkFile db (File name' B.empty)
  newFileIdFromName db name'


newFileIdFromName ∷ DB → FileName → IO FileId
newFileIdFromName db name = do
  maybeFileEntity ← fileEntityNamed db name
  case maybeFileEntity of
    Nothing →
      error "We just created the file; should exist."
    Just (FileEntity fileId (File _ _)) →
      return fileId

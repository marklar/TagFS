{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module File.Create
  ( tCreateDevice
  , tCreateLink
  ) where

import qualified Data.ByteString.Char8   as B
import           System.Fuse
import           Data.Maybe              (catMaybes)
import           System.Posix.Types      (FileMode, DeviceID)
import           System.FilePath.Posix   (splitExtension, addExtension)

import           Debug                   (dbg)
import           DB.Base
import           DB.Read                 (fileEntityNamed, fileEntityFromPath)
import           DB.Write
import           File.Util               (tagFile)  -- move to DB.Write?
import           File.Version            (versionedFileName)
import           Parse
import           Stat.Base               (dirStat, fileStat)
import           Types


{- | Doesn't actually create a link. Instead, it adds tags to an
   existing file.
-}
tCreateLink ∷ DB → FilePath → FilePath → IO Errno
tCreateLink db fromPath toPath = do
  dbg $ "CreateLink: " ++ fromPath ++ ", " ++ toPath

  -- What we want is for fromPath and toPath to be:
  --  + alike in name, but
  --  + different in tags.

  if pathsDiffer && namesMatch
    -- Add toTags which file may currently lack.
    then do maybeFileEntity ← fileEntityFromPath db fromPath
            case maybeFileEntity of
              Nothing → return eNOENT
              Just (FileEntity fileId _) → do
                tagFile db fileId toTags
                return eOK
    else return eINVAL -- eINVAL | ePERM | eACCES
    -- eACCES - permission denied
    -- eINVAL - invalid argument
    -- ePERM  - operation not permitted
    -- http://www.virtsync.com/c-error-codes-include-errno

  where
    pathsDiffer = fromPath /= toPath
    namesMatch = maybeFromName == maybeToName
    (fromTags, maybeFromName) = parseFilePath fromPath
    (toTags,   maybeToName)   = parseFilePath toPath


----------------------------


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

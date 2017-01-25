{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module FuseOps where

import           System.Fuse
import           System.Posix.Types      (EpochTime)

import           Debug                   (dbg)
import           DB.Base                 (DB)
import           Dir.Base                (openDir, removeDir)
import           Dir.Create              (createDir)
import           Dir.Read                (readDir)
import           File.Base               (tOpenFile, tReadFile, tSetFileTimes)
import           File.Create             (tCreateDevice)
import           File.Remove             (tRemoveLink)
import           File.Rename             (tRenameFile)
import           File.Write              (tWriteFile, setFileSize)
import           Stat.Base               (getFileSystemStats)
import           Stat.File               (getFileStat)
import           Types



runFuse ∷ DB → IO ()
runFuse db = do
  ctx ← getFuseContext
  dbg "------------------------------------------------"
  fuseMain fuseOps defaultExceptionHandler
  where
    fuseOps ∷ FuseOperations NonHandle
    fuseOps =
      defaultFuseOps
      { fuseGetFileSystemStats = getFileSystemStats

      -- DIR
      , fuseCreateDirectory    = createDir db
      , fuseOpenDirectory      = openDir   db
      , fuseRemoveDirectory    = removeDir db
      , fuseReadDirectory      = readDir   db

      -- FILE
      , fuseCreateDevice       = tCreateDevice db
      , fuseOpen               = tOpenFile     db
      , fuseRead               = tReadFile     db
      , fuseWrite              = tWriteFile    db
      , fuseRename             = tRenameFile   db
      , fuseSetFileMode        = \_ _ → return eOK
      -- fuseSetOwnerAndGroup
      , fuseSetFileSize        = setFileSize   db

      -- Any kind of entry (node)
      , fuseGetFileStat        = getFileStat   db
      -- , fuseAccess             = tAccess
      , fuseRemoveLink         = tRemoveLink   db
      -- , fuseSetFileTimes       = tSetFileTimes db

      -- LINKS
      -- fuseReadSymbolicLink
      -- fuseCreateSymbolicLink
      -- fuseCreateLink

      -- FLUSH
      -- fuseFlush

      -- RELEASE
      , fuseRelease            = \_ _ → return ()
      -- fuseReleaseDirectory

      -- SYNCHRONIZE
      -- fuseSynchronizeFile
      -- fuseSynchronizeDirectory

      -- SETUP, TAKEDOWN
      -- fuseInit
      -- fuseDestroy
      }



--------------------


{- | This is the same as the access(2) system call.

It returns:
  + eNOENT  - if the path doesn't exist
  + eACCESS - if the requested permission isn't available
  + eOK     - for success

Can be called on files, dirs, or any other object in the FS.
-}
tAccess ∷ FilePath → Int → IO Errno
tAccess filePath _ = do
  dbg $ "Access: " ++ filePath
  return eOK


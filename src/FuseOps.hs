{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module FuseOps where

import           System.Fuse
import           System.Posix.Types      (EpochTime)

import           Debug                   (dbg)
import           Device                  (tCreateDevice)
import           DB.Model                (DB)
import           Dir.Base                (openDir, removeDir)
import           Dir.Create              (createDir)
import           Dir.Read                (readDir)
import           File                    (tOpenFile, tReadFile, tWriteFile)
import           FileStat                (getFileStat)
import           Remove                  (tRemoveLink)
import           Stat                    (getFileSystemStats)
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
      { fuseGetFileSystemStats = Stat.getFileSystemStats

      -- Dir
      , fuseCreateDirectory    = Dir.Create.createDir db
      , fuseOpenDirectory      = Dir.Base.openDir   db
      , fuseRemoveDirectory    = Dir.Base.removeDir db
      , fuseReadDirectory      = Dir.Read.readDir   db

      -- File
      , fuseOpen               = File.tOpenFile  db
      , fuseRead               = File.tReadFile  db
      , fuseWrite              = File.tWriteFile db

      -- Any kind of entry (node)
      , fuseGetFileStat        = getFileStat   db
      -- , fuseAccess             = tAccess
      , fuseCreateDevice       = tCreateDevice db
      , fuseRemoveLink         = tRemoveLink   db
      -- , fuseSetFileTimes       = tSetFileTimes db

      -- fuseReadSymbolicLink
      -- fuseCreateSymbolicLink
      -- fuseRename
      -- fuseCreateLink
      -- fuseSetFileMode
      -- fuseSetOwnerAndGroup
      -- fuseSetFileSize
      -- fuseFlush
      , fuseRelease            = \_ _ → return ()
      -- fuseSynchronizeFile
      -- fuseReleaseDirectory
      -- fuseSynchronizeDirectory
      -- fuseInit
      -- fuseDestroy
      }



--------------------


tSetFileTimes ∷ DB → FilePath → EpochTime → EpochTime → IO Errno
tSetFileTimes db filePath t1 t2 = return eOK


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


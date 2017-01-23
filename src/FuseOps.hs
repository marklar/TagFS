{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module FuseOps where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B
import           Data.Maybe              (catMaybes)
import           Data.Monoid             ((<>))

import           System.Fuse
import           System.IO
import           System.Posix.Files
import           System.Posix.Types

import           Debug                   (dbg)
import           Device                  (tCreateDevice)
import           DB.Model
import           DB.Find                 -- (filesFromTags)
import           DB.Insert
import           Dir                     (createDir, openDir, readDir)
import           Node                    (nodeNamed)
import           File                    ( tOpenFile, tReadFile, tWriteFile
                                         , fileEntityFromPath  -- TODO: mv to utils
                                         )
import           Parse
import           Stat                    ( dirStat, fileStat
                                         , getFileSystemStats
                                         )
import           Types



runFuse ∷ DB → IO ()
runFuse db = do
  ctx ← getFuseContext
  fuseMain fuseOps defaultExceptionHandler
  where
    fuseOps ∷ FuseOperations NonHandle
    fuseOps =
      defaultFuseOps
      { fuseGetFileSystemStats = Stat.getFileSystemStats

      -- Dir
      , fuseCreateDirectory    = Dir.createDir db
      , fuseOpenDirectory      = Dir.openDir   db
      , fuseReadDirectory      = Dir.readDir   db
      -- fuseRemoveDirectory

      -- File
      , fuseOpen               = File.tOpenFile  db
      , fuseRead               = File.tReadFile  db
      , fuseWrite              = File.tWriteFile db

      -- Either
      , fuseGetFileStat        = getFileStat   db
      -- , fuseAccess             = tAccess
      , fuseCreateDevice       = tCreateDevice db
      , fuseRemoveLink         = tRemoveLink   db
      , fuseSetFileTimes       = tSetFileTimes db

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


{- | getattr: info about inode (number, owner, last access)
   Should work for either Files or Dirs.
   (Perhaps 'FileStat' should be renamed to 'NodeStat'?)
-}
getFileStat ∷ DB → FilePath → IO (Either Errno FileStat)
getFileStat db filePath = do
  dbg $ "GetFileStat: " ++ filePath
  ctx ← getFuseContext

  case filePath of

    -- What the hell is this?
    "/._." →
      return $ Left eNOENT

    -- Root dir: show all files & tags.
    "/" → do
      return $ Right (dirStat ctx)

    -- File or Dir?
    _ → do
      maybeFileEntity ← fileEntityFromPath db filePath
      case maybeFileEntity of

        -- TODO: store stat info w/ file & return it here.
        Just (FileEntity _ _) →
          return $ Right (fileStat ctx)

        Nothing → do
          -- error "need a function that recursively looks up stats"
          fileEntities ← filesFromTags db (parseDirPath filePath)
          if null fileEntities
            then return $ Left eNOENT
            else return $ Right (dirStat ctx)


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


--------------------


{- | unlink(const char* path)

   Remove (delete) the given file, symbolic link, hard link, or
   special node. Note that if you support hard links, unlink only
   deletes the data when the last hard link is removed. See unlink(2)
   for details.
-}
tRemoveLink ∷ DB → FilePath → IO Errno
tRemoveLink db filePath = do
  dbg $ "RemoveLink: " ++ filePath
  let (_:fileName) = filePath
  
  maybeNode ← nodeNamed db fileName
  case maybeNode of

    Nothing →
      return eNOENT

    Just _ → do
      rmNodeByName db fileName
      return eOK


rmNodeByName ∷ DB → FileName → IO ()
rmNodeByName db fileName = undefined

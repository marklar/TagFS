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
import           DB.Model
import           DB.Find                 -- (filesFromTags)
import           DB.Insert
import           Dir                     (createDir, openDir, readDir)
import           Node                    (nodeNamed)
import           File                    ( tOpenFile
                                         , tReadFile
                                         , tWriteFile
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


{- | getattr(const char* path, struct stat* stbuf)

Return file attributes. The "stat" structure is described in detail in
the stat(2) manual page. For the given pathname, this should fill in
the elements of the "stat" structure. If a field is meaningless or
semi-meaningless (e.g., st_ino) then it should be set to 0 or given a
"reasonable" value. This call is pretty much required for a usable
filesystem.
-}

{- | getattr: info about inode (number, owner, last access)
   Should work for either Files or Dirs.
   (Perhaps 'FileStat' should be renamed to 'NodeStat'?)
-}
getFileStat ∷ DB → FilePath → IO (Either Errno FileStat)
getFileStat db filePath = do
  dbg $ "GetFileStat: " ++ filePath
  case filePath of

    -- What the hell is this?
    "/._." →
      return $ Left eNOENT

    -- Root dir: Special case, as it's the absence of any tags.
    "/" → do
      ctx ← getFuseContext
      return $ Right (dirStat ctx)

    _ → do
      dbg "  Might be Dir, might be File. Trying first as File."
      maybeFileEntity ← fileEntityFromPath db filePath
      case maybeFileEntity of

        Just (FileEntity _ _) → do
          -- dbg "  Found file"
          ctx ← getFuseContext
          return $ Right (fileStat ctx)

        Nothing → do
          -- error "need a function that recursively looks up stats"
          let tagNames = parseDirPath filePath
          dbg $ "  Not a File. Trying as Dir w/ tagNames: " ++ show tagNames
          fileEntities ← filesFromTags db tagNames
          dbg $ "  filesFromTags: " ++ show fileEntities
          if null fileEntities
            then return $ Left eNOENT
            else do dbg "  Found dir"
                    ctx ← getFuseContext
                    return $ Right (dirStat ctx)


--------------------


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
  ctx ← getFuseContext

  case entryType of

    RegularFile → do
      let newStat = (fileStat ctx) { statFileMode = mode }    -- record update syntax
      let (tagNames, maybeFileName) = parseFilePath filePath

      case maybeFileName of
        Nothing →
          return eNOENT

        Just n → do
          mkFile db (File n B.empty)
          maybeFileEntity ← fileEntityNamed db n

          case maybeFileEntity of
            Nothing →
              return eNOENT  -- Should never happen!

            Just (FileEntity fileId (File _ _)) → do
              maybeTagEntities ← mapM (tagEntityNamed db) tagNames
              let tagIds = map tagId (catMaybes maybeTagEntities)
              mapM_ (mkFileTag db fileId) tagIds
              return eOK

    _ → do
      dbg $ "Failed to create unknown device type with path: " ++ filePath
      return eNOENT


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

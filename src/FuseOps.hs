{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module FuseOps where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B
import           Data.Monoid             ((<>))

import           System.Fuse
import           System.IO
import           System.Posix.Files
import           System.Posix.Types

import           Debug                   (dbg)
import           DB.Model
import           Dir                     (createDir, openDir, readDir)
import           Node                    (nodeNamed)
import           File                    (tOpenFile, tReadFile, tWriteFile)
import           Parse
import           Stat                    ( dirStat, fileStat
                                         , getFileSystemStats
                                         )
import           Types


-- Question: db the file contents in the DB, or use the actual FS?
-- If in the actual FS, how?

--------------------

runFuse ∷ DB → IO ()
runFuse db = do
  dbg "getting FUSE context"
  ctx ← getFuseContext

  dbg "before fuseMain"
  fuseMain fuseOps defaultExceptionHandler
  dbg "after fuseMain"

  where
    fuseOps ∷ FuseOperations NonHandle
    fuseOps = defaultFuseOps
              { fuseGetFileSystemStats = Stat.getFileSystemStats

              -- Dir
              , fuseCreateDirectory    = Dir.createDir db
              , fuseOpenDirectory      = Dir.openDir db
              , fuseReadDirectory      = Dir.readDir db
              -- fuseRemoveDirectory

              -- File
              , fuseOpen               = File.tOpenFile db
              , fuseRead               = File.tReadFile db
              , fuseWrite              = File.tWriteFile db

              -- Either
              , fuseGetFileStat        = getFileStat db
              , fuseAccess             = tAccess
              , fuseCreateDevice       = tCreateDevice db
              , fuseRemoveLink         = tRemoveLink db
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

    -- Root dir:
    -- Special case, as it's the absence of any tags.
    -- But are we allowed to have any untagged files?
    -- Or do we show all the files as part of the root dir?
    "/" → do
      ctx ← getFuseContext
      dbg "  for '/'"
      return $ Right (dirStat ctx)


    -- Look up just by the fileName
    _ → do

      -- FIXME: filePath might be *either*:
      --   + just a directory (e.g. "sports/packers")
      --   + a file (e.g. "sports/packers/football.txt"
      let (fileName:restOfPath) = pathParts filePath
      dbg $ "  finding node of name: " ++ fileName
      maybeNode ← nodeNamed db fileName
      case maybeNode of

        Nothing → do
          dbg $ "  Failed to find " ++ fileName
          return (Left eNOENT)

        Just (FileNode stat _) → do
          dbg "  Found file"
          return (Right stat)

        Just (DirNode stat) → do
          dbg "  Found dir"
          -- error "need a function that recursively looks up stats"
          return (Right stat)


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
  dbg $ "creating device with path: " ++ filePath
  ctx ← getFuseContext

  case entryType of

    RegularFile → do
      let (fileName:restOfPath) = pathParts filePath
      let newStat = (fileStat ctx) { statFileMode = mode }    -- record update syntax
      createNewFile db fileName "" newStat
      return eOK

    _ → do
      dbg $ "Failed to create unknown device type with path: " ++ filePath
      return eNOENT


-- TODO
createNewFile ∷ DB → FileName → ByteString → FileStat → IO ()
createNewFile db fileName contents fStat = undefined


--------------------


tSetFileTimes ∷ DB → FilePath → EpochTime → EpochTime → IO Errno
tSetFileTimes db filePath t1 t2 = return eOK


{- | access(const char* path, mask)

This is the same as the access(2) system call. It returns -ENOENT if
the path doesn't exist, -EACCESS if the requested permission isn't
available, or 0 for success. Note that it can be called on files,
directories, or any other object that appears in the filesystem. This
call is not required but is highly recommended.
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

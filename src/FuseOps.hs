{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module FuseOps where

import           Database.HDBC
import           Database.HDBC.Sqlite3

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B
import           Data.List.Split         (splitOn)
import           Data.Maybe
import           Control.Monad
import           Data.Monoid             ((<>))

import           System.Fuse
import           System.IO
import           System.Posix.Files
import           System.Posix.Types

import           Find
import           Model
import           Stat                    ( dirStat, fileStat
                                         , tagGetFileSystemStats
                                         )


-- Question: store the file contents in the DB, or use the actual FS?
-- If in the actual FS, how?

data NonHandle = NonHandle


-- Debugging. Instrument fns w/ log output to see what's happening.
-- IMPORTANT!!! Must be absolute path.
debugFile ∷ FilePath
debugFile = "/Users/markwong-vanharen/Development/TagFS/debug.log"

dbg ∷ ByteString → IO ()
dbg msg = B.appendFile debugFile (msg <> "\n")


-- Each Entry, whether Dir or File, has a FileStat (from FUSE).
-- Files also have contents.
data Entry = FileEntry FileStat !ByteString
           | DirEntry  FileStat
  deriving Show


--------------------

runFuse ∷ Connection → IO ()
runFuse dbConn = do
  dbg "getting FUSE context"
  ctx ← getFuseContext

  dbg "before fuseMain"
  fuseMain fuseOps defaultExceptionHandler
  dbg "after fuseMain"

  where
    fuseOps ∷ FuseOperations NonHandle
    fuseOps = defaultFuseOps
              { fuseGetFileStat        = tagGetFileStat dbConn
              -- fuseReadSymbolicLink
              , fuseCreateDevice       = tagCreateDevice dbConn
              , fuseCreateDirectory    = tagCreateDir dbConn
              , fuseRemoveLink         = tagRemoveLink dbConn
              -- fuseRemoveDirectory
              -- fuseCreateSymbolicLink
              -- fuseRename
              -- fuseCreateLink
              -- fuseSetFileMode
              -- fuseSetOwnerAndGroup
              -- fuseSetFileSize
              , fuseSetFileTimes       = tagSetFileTimes dbConn
              , fuseOpen               = tagOpen
              , fuseRead               = tagRead dbConn
              , fuseWrite              = tagWrite dbConn
              , fuseGetFileSystemStats = tagGetFileSystemStats
              -- fuseFlush
              , fuseRelease            = \_ _ → return ()
              -- fuseSynchronizeFile
              , fuseOpenDirectory      = tagOpenDir dbConn
              , fuseReadDirectory      = tagReadDir dbConn
              -- fuseReleaseDirectory
              -- fuseSynchronizeDirectory
              , fuseAccess             = tagAccess
              -- fuseInit
              -- fuseDestroy
              }

--------------------


{- | https://www.cs.hmc.edu/~geoff/classes/hmc.cs135.201001/homework/fuse/fuse_doc.html

open(const char* path, struct fuse_file_info* fi)

Open a file. If you aren't using file handles, this function should
just check for existence and permissions and return either success or
an error code. If you use file handles, you should also allocate any
necessary structures and set fi->fh. In addition, fi has some other
fields that an advanced filesystem might find useful; see the
structure definition in fuse_common.h for very brief commentary.
-}

{- | Does nothing, really. Since this action uses an Either, it returns
   a Right of a filehandle-like value. That FH-like value is ignored
   by FUSE. Fuse simply passes back to us to use in other operations.
-}
tagOpen ∷ FilePath
        → OpenMode         -- FUSE: ReadOnly | WriteOnly | ReadWrite
        → OpenFileFlags    -- FUSE: append | exclusive | noctty | nonBlock | trunc
        → IO (Either Errno NonHandle)
tagOpen filePath mode flags = do
  let (_:fileName) = filePath
  dbg $ "Opening " <> B.pack fileName
  return (Right NonHandle)


--------------------

{- | https://www.cs.hmc.edu/~geoff/classes/hmc.cs135.201001/homework/fuse/fuse_doc.html

read(const char* path, char *buf, size_t size, off_t offset, struct fuse_file_info* fi)

Read sizebytes from the given file into the buffer buf, beginning
offset bytes into the file. See read(2) for full details. Returns the
number of bytes transferred, or 0 if offset was at or beyond the end
of the file. Required for any sensible filesystem.
-}

{- | In case of success, return 'bc' bytes of the ByteString contents of
   a file.
-}
tagRead ∷ Connection   -- db connection
        → FilePath
        → NonHandle
        → ByteCount    -- System.Posix.Types
        → FileOffset   -- System.Posix.Types
        → IO (Either Errno ByteString)
tagRead dbConn filePath handle bc offset = do
  let (_:fileName) = filePath
  dbg $ "Reading: " <> B.pack fileName

  maybeEntry ← findFileByName dbConn fileName
  case maybeEntry of

    Just (FileEntry fStat contents) → do
      dbg $ "  Read: " <> B.pack fileName <> ", got: " <> contents
      return (Right contents)

    Nothing → do
      dbg $ "  Read: failed on " <> B.pack fileName
      return (Left eNOENT)


-------------------


findEntryByName ∷ Connection → String → IO (Maybe Entry)
findEntryByName dbConn name = do
  maybeFile ← findFileByName dbConn name
  if isJust maybeFile
    then return maybeFile
    else findTagByName dbConn name


findFileByName ∷ Connection → String → IO (Maybe Entry)
findFileByName dbConn name =
  fileFromName dbConn name >>= entityToEntry


findTagByName ∷ Connection → String → IO (Maybe Entry)
findTagByName dbConn name =
  tagFromName dbConn name >>= entityToEntry


entityToEntry ∷ Maybe Entity → IO (Maybe Entry)
entityToEntry maybeEntity = do
  ctx ← getFuseContext
  case maybeEntity of
    Just (FileEntity _ (File _ contents)) →
      return $ Just $ FileEntry (fileStat ctx) contents
    Just (TagEntity _ (Tag _)) →
      return $ Just $ DirEntry (dirStat ctx)
    Nothing →
      return Nothing
  
  
--------------------


{- | https://www.cs.hmc.edu/~geoff/classes/hmc.cs135.201001/homework/fuse/fuse_doc.html

readdir(const char* path, void* buf, fuse_fill_dir_t filler, off_t offset, struct fuse_file_info* fi)

Return one or more directory entries (struct dirent) to the
caller. … [Lots of stuff elided.] …

It's also important to note that readdir can return errors in a number
of instances; in particular it can return -EBADF if the file handle is
invalid, or -ENOENT if you use the path argument and the path doesn't
exist.

-}

{- | Entire contents of dir.
-}
tagReadDir :: Connection → FilePath
           → IO (Either Errno [(FilePath, FileStat)])
tagReadDir dbConn filePath = do
  dbg $ "ReadDir: " <> B.pack filePath

  ctx ← getFuseContext
  return $ Right [ (".",  (dirStat ctx))
                 , ("..", (dirStat ctx))
                 ]
  -- filesAndStats ← getFilesAndStats dbConn filePath
  -- return $ case filesAndStats of
  --            [] → Left eNOENT
  --            _  → Right filesAndStats


-- TODO
getFilesAndStats ∷ Connection → FilePath → IO [(FilePath, FileStat)]
getFilesAndStats dbConn filePath = undefined


--------------------

{- | If asked to create a RegularFile, create an empty one w/ provided
   mode & return eOK. If some other type of device: eNOENT.
-}
tagCreateDevice ∷ Connection
                → FilePath    -- FilePath ~ String
                → EntryType   -- FUSE: The Unix type of a node in the FS (RegularFile | Directory | …)
                → FileMode    -- System.Posix.Types
                → DeviceID    -- System.Posix.Types
                → IO Errno    -- Foreign.C.Error
tagCreateDevice dbConn filePath entryType mode deviceId = do
  dbg $ "creating device with path: " <> B.pack filePath
  ctx ← getFuseContext

  case entryType of

    RegularFile → do
      let (fileName:restOfPath) = pathParts filePath
      let newStat = (fileStat ctx) { statFileMode = mode }    -- record update syntax
      createNewFile dbConn fileName "" newStat
      return eOK

    _ → do
      dbg $ "Failed to create unknown device type with path: " <> B.pack filePath
      return eNOENT


pathParts ∷ FilePath → [String]
pathParts filePath =
  filter (/= "") . splitOn "/" $ filePath


-- TODO
createNewFile ∷ Connection → FileName → ByteString → FileStat → IO ()
createNewFile dbConn fileName contents fStat = undefined


--------------------

-- TODO
{- | What's in FileName? So we know how to use it to create tag(s).
-}
createNewTags ∷ Connection → FileName → FileStat → IO ()
createNewTags dbConn fileName fStat = undefined


{- | Create new tags (as necessary) for dirs in path.
-}
tagCreateDir ∷ Connection → FilePath → FileMode → IO Errno
tagCreateDir dbConn filePath mode = do
  dbg $ "creating directory with path: " <> B.pack filePath

  ctx ← getFuseContext
  let (_:fileName) = filePath
  let newStat = (dirStat ctx) { statFileMode = mode }
  createNewTags dbConn fileName newStat
  return eOK



--------------------

{- | If filePath maps to a dir: eOK. Else: eNOENT.
-}
tagOpenDir ∷ Connection → FilePath → IO Errno
tagOpenDir dbConn filePath = do
  dbg $ "Opening dir " <> B.pack filePath
  if filePath == "/"
    then return eOK
    else do let (_:fileName) = filePath
            -- FIXME: Logic is too simple.
            -- Check whether any files have this combo of tags.
            ex ← tagExists dbConn fileName
            if ex
              then return eOK
              else return eNOENT


--------------------

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
tagGetFileStat ∷ Connection → FilePath → IO (Either Errno FileStat)
tagGetFileStat dbConn filePath = do
  dbg $ "GetFileStat: " <> B.pack filePath

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
      let (fileName:restOfPath) = pathParts filePath
      dbg $ "  finding file of name: " <> B.pack fileName
      
      maybeEntry ← findEntryByName dbConn fileName
      
      case maybeEntry of

        Nothing → do
          dbg $ "  Failed to find " <> B.pack fileName
          return (Left eNOENT)

        Just (FileEntry stat _) → do
          dbg "  Found file"
          return (Right stat)

        Just (DirEntry stat) → do
          dbg "  Found dir"
          error "need a function that recursively looks up stats"


--------------------


tagWrite ∷ Connection
         → FilePath
         → NonHandle
         → ByteString
         → FileOffset
         → IO (Either Errno ByteCount)
tagWrite dbConn filePath _ bytes offset = do
  dbg $ "Write: " <> B.pack filePath

  let (_:fileName) = filePath

  maybeEntity ← findFileByName dbConn fileName
  case maybeEntity of
    
    Nothing → do
      dbg $ "  didn't find file (" <> B.pack fileName <> ")"
      return (Left eNOENT)

    Just (DirEntry _) → do
      dbg $ "  found dir (" <> B.pack fileName <> ")"
      return (Left eNOENT)
      
    Just (FileEntry fStat contents) → do
      dbg $ "  found: -- " <> B.pack fileName
      writeFile fStat fileName contents

  where
    writeFile fStat fileName contents = do
      let contents' = B.take ((fromIntegral offset) - 1) contents <> bytes
          fileSize = fromIntegral . B.length $ contents'
          fileEntry = FileEntry (fStat { statFileSize = fileSize }) contents'
      return $ Right (fromIntegral . B.length $ bytes)


--------------------


tagSetFileTimes ∷ Connection → FilePath → EpochTime → EpochTime → IO Errno
tagSetFileTimes dbConn filePath t1 t2 = return eOK


tagAccess ∷ FilePath → Int → IO Errno
tagAccess filePath _ = do
  dbg $ "Access: " <> B.pack filePath
  return eOK


--------------------


{- | unlink(const char* path)

   Remove (delete) the given file, symbolic link, hard link, or
   special node. Note that if you support hard links, unlink only
   deletes the data when the last hard link is removed. See unlink(2)
   for details.
-}
tagRemoveLink ∷ Connection → FilePath → IO Errno
tagRemoveLink dbConn filePath = do
  dbg $ "RemoveLink: " <> B.pack filePath
  let (_:fileName) = filePath
  
  maybeEntry ← findEntryByName dbConn fileName
  case maybeEntry of

    Nothing →
      return eNOENT

    Just _ → do
      rmEntryByName dbConn fileName
      return eOK


rmEntryByName ∷ Connection → FileName → IO ()
rmEntryByName dbConn fileName = undefined

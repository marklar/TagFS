{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module File
  ( tOpenFile
  , tReadFile
  , tWriteFile
  ) where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B
import           Data.Monoid             ((<>))

import           System.Fuse
import           System.IO
import           System.Posix.Files
import           System.Posix.Types

import           DB.Find
import           DB.Model
import           Debug                   (dbg)
import           Node                    (fileNodeNamed)
import           Parse
import           Types


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
tOpenFile ∷ FilePath
          → OpenMode         -- FUSE: ReadOnly | WriteOnly | ReadWrite
          → OpenFileFlags    -- FUSE: append | exclusive | noctty | nonBlock | trunc
          → IO (Either Errno NonHandle)
tOpenFile filePath mode flags = do
  let (_:fileName) = filePath
  dbg $ "Opening " ++ fileName
  return (Right NonHandle)


------------------------


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
tReadFile ∷ DB   -- db connection
          → FilePath
          → NonHandle
          → ByteCount    -- System.Posix.Types
          → FileOffset   -- System.Posix.Types
          → IO (Either Errno ByteString)
tReadFile db filePath handle bc offset = do
  let (_:fileName) = filePath
  dbg $ "Reading: " ++ fileName

  maybeNode ← fileNodeNamed db fileName
  case maybeNode of

    Just (FileNode fStat contents) → do
      dbg $ "  Read: " ++ fileName ++ ", got: " ++ B.unpack contents
      return (Right contents)

    Nothing → do
      dbg $ "  Read: failed on " ++ fileName
      return (Left eNOENT)


------------------------


tWriteFile ∷ DB
           → FilePath
           → NonHandle
           → ByteString
           → FileOffset
           → IO (Either Errno ByteCount)
tWriteFile db filePath _ bytes offset = do
  dbg $ "Write: " ++ filePath

  let (_:fileName) = filePath

  maybeEntity ← fileNodeNamed db fileName
  case maybeEntity of
    
    Nothing → do
      dbg $ "  didn't find file (" ++ fileName ++ ")"
      return (Left eNOENT)

    Just (DirNode _) → do
      dbg $ "  found dir (" ++ fileName ++ ")"
      return (Left eNOENT)
      
    Just (FileNode fStat contents) → do
      dbg $ "  found: -- " ++ fileName
      writeFile fStat fileName contents

  where
    writeFile fStat fileName contents = do
      let contents' = B.take ((fromIntegral offset) - 1) contents <> bytes
          fileSize = fromIntegral . B.length $ contents'
          fileNode = FileNode (fStat { statFileSize = fileSize }) contents'
      return $ Right (fromIntegral . B.length $ bytes)

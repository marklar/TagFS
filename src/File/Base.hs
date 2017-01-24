{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module File.Base
  ( tOpenFile
  , tReadFile
  , tWriteFile
  , tSetFileTimes
  ) where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B
import           Data.Maybe              (isJust)
import           Data.Monoid             ((<>))

import           System.Fuse
import           System.IO
import           System.Posix.Files
import           System.Posix.Types

import           DB.Base
import           DB.Read
import           DB.Write                (updateFile)
import           Debug                   (dbg)
import           Node                    (fileNodeNamed)
import           Parse
import           Stat.Base               (contentsFileStat)
import           Types


{- | Open a file. Just check for existence and permissions and return
   either success or an error code.

   Success here means a Right of a filehandle-like value. That FH-like
   value is ignored by FUSE. Fuse simply passes back to us to use in
   other operations.
-}
tOpenFile ∷ DB
          → FilePath
          → OpenMode         -- FUSE: ReadOnly | WriteOnly | ReadWrite
          → OpenFileFlags    -- FUSE: append | exclusive | noctty | nonBlock | trunc
          → IO (Either Errno NonHandle)
tOpenFile db filePath mode flags = do
  dbg $ "OpenFile: " ++ filePath
  maybeEntity ← fileEntityFromPath db filePath
  if isJust maybeEntity
    then do dbg "  found"
            return $ Right NonHandle
    else do dbg "  not found"
            return $ Left eNOENT


------------------------


{- | First, check for existence and readability.

   If readable, read 'bc' bytes from the given file starting at
   'offset', and return as a ByteString.
-}
tReadFile ∷ DB
          → FilePath
          → NonHandle
          → ByteCount    -- System.Posix.Types
          → FileOffset   -- System.Posix.Types
          → IO (Either Errno ByteString)
tReadFile db filePath _ bc offset = do
  dbg $ "ReadFile: " ++ filePath
  maybeEntity ← fileEntityFromPath db filePath
  case maybeEntity of
    Nothing →
      return $ Left eNOENT
    Just (FileEntity _ (File _ contents)) → do
      -- FIXME: Check readability permissions.
      return .
        Right .
        B.take (fromIntegral bc) .
        B.drop (fromIntegral offset) $
        contents


------------------------


{- | If find file (in proper dir), update its contents.
-}
tWriteFile ∷ DB
           → FilePath
           → NonHandle
           → ByteString
           → FileOffset
           → IO (Either Errno ByteCount)
tWriteFile db filePath _ bytes offset = do
  dbg $ "WriteFile: " ++ filePath

  maybeEntity ← fileEntityFromPath db filePath
  case maybeEntity of
    Nothing → do
      dbg $ "  didn't find file (" ++ filePath ++ ")"
      return (Left eNOENT)

    Just (FileEntity fileId (File name contents)) → do
      dbg $ "  found: -- " ++ name
      -- FIXME: need to store fStat in DB w/ file
      ctx ← getFuseContext
      writeFile (contentsFileStat ctx contents) (File name contents)

  where
    writeFile fStat (File name contents) = do
      let contents' = B.take ((fromIntegral offset) - 1) contents <> bytes
          fileSize = fromIntegral . B.length $ contents'
          -- fileNode = FileNode (fStat { statFileSize = fileSize }) contents'
      updateFile db (File name contents')
      return $ Right (fromIntegral . B.length $ bytes)


--------------


tSetFileTimes ∷ DB → FilePath → EpochTime → EpochTime → IO Errno
tSetFileTimes db filePath t1 t2 = return eOK


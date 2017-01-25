{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module File.Write
  ( tWriteFile
  , setFileSize
  ) where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B
import           Data.Monoid             ((<>))

import           System.Fuse
-- import           System.IO
-- import           System.Posix.Files
import           System.Posix.Types

import           DB.Base
import           DB.Read
import           DB.Write                (updateContents)
import           Debug                   (dbg)
import           Stat.Base               (contentsFileStat)
import           Types


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
  writeFileHelper db filePath bytes offset


writeFileHelper ∷ DB
                → FilePath
                → ByteString
                → FileOffset
                → IO (Either Errno ByteCount)
writeFileHelper db filePath bytes offset = do
  maybeEntity ← fileEntityFromPath db filePath
  case maybeEntity of
    Nothing → do
      dbg $ "  didn't find file (" ++ filePath ++ ")"
      return (Left eNOENT)

    --(FileEntity fileId (File name contents))
    Just fileEntity → do
      dbg "  found file"
      -- FIXME: need to store fStat in DB w/ file
      ctx ← getFuseContext
      -- writeFile (contentsFileStat ctx contents) (File name contents)
      bc ← writeFileAt db fileEntity bytes offset
      return $ Right bc


-----------------


setFileSize ∷ DB → FilePath → FileOffset → IO Errno
setFileSize db filePath offset = do
  dbg $ "SetFileSize: " ++ filePath
  r ← writeFileHelper db filePath "" offset
  case r of
    Left e →
      return e
    Right bc →
      return eOK


------------------


writeFileAt ∷ DB → Entity → ByteString → FileOffset → IO ByteCount
writeFileAt db (FileEntity fileId (File name contents)) bytes offset = do
  -- TODO: use the fileId (from above), rather than name.
  let contents' = B.take ((fromIntegral offset) - 1) contents <> bytes
      fileSize = fromIntegral . B.length $ contents'
      -- fileNode = FileNode (fStat { statFileSize = fileSize }) contents'
  updateContents db (File name contents')
  return $ fromIntegral . B.length $ bytes

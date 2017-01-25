{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module File.Remove where

import           System.Fuse

import           DB.Base
import           DB.Read                 (fileEntityFromPath)
import           Debug                   (dbg)
import           Node                    (nodeNamed)
import           Parse
import           File.Util               (rmLastTag)


{- | unlink(const char* path)
   Remove (delete) the given file, symbolic link, hard link, or
   special node.
-}
tRemoveLink ∷ DB → FilePath → IO Errno
tRemoveLink db filePath = do
  dbg $ "RemoveLink: " ++ filePath

  maybeEntity ← fileEntityFromPath db filePath
  case maybeEntity of
    Nothing →
      return eNOENT

    Just (FileEntity fileId _) → do
      rmLastTag db fileId filePath
      return eOK

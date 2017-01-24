{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Remove where

import           System.Fuse

import           DB.Base
import           DB.Read                 (fileEntityFromPath)
import           DB.Write                (rmFile, rmFileTag)
import           Debug                   (dbg)
import           Node                    (nodeNamed)
import           Parse


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

    Just (FileEntity fileId (File fileName contents)) → do
      let (tagNames, _) = parseFilePath filePath
      if null tagNames
        then rmFile db fileId   -- & all associated FileTags
        else rmFileTag db fileId (last tagNames)
      return eOK

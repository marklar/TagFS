{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Remove where

import           System.Fuse

import           DB.Base
import           Debug                   (dbg)
import           Node                    (nodeNamed)


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

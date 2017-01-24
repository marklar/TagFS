{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module File.Rename
  ( tRenameFile
  ) where


import           System.Fuse
import           DB.Base
import           DB.Read              (fileEntityFromPath)
import           DB.Write             (rmFileTag)

import           Debug
import           File.Util            (tagFile)
import           Parse                (parseFilePath)


{- | For 'mv'. Delete + Copy.
-}
tRenameFile ∷ DB → FilePath → FilePath → IO Errno
tRenameFile db fromPath toPath = do
  dbg $ "RenameFile: " ++ fromPath ++ " " ++ toPath

  -- e.g. /simon.txt /barca/simon.txt

  maybeEntity ← fileEntityFromPath db fromPath
  case maybeEntity of
    Nothing →
      return eNOENT

    Just (FileEntity fileId _) → do
      -- 1. For last tagName of fromPath (if any), rm FileTag.
      let (tagNames, _) = parseFilePath fromPath
      if null tagNames
        then return ()
        else rmFileTag db fileId (last tagNames)

      -- 2. For all tags in toPath, make new FileTags.
      let (toTagNames, _) = parseFilePath toPath
      tagFile db fileId toTagNames
      return eOK

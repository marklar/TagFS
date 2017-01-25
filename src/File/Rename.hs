{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module File.Rename
  ( tRenameFile
  ) where


import           System.Fuse
import           DB.Base
import           DB.Read              (fileEntityFromPath)
import           DB.Write             (rmFileTag, renameFile)

import           Debug
import           File.Util            (tagFile)
import           Parse                (parseFilePath)


data Move = Retagging  -- ^ mv /a/y.txt /b/y.txt   (same name, diff dir)
          | Renaming   -- ^ mv /a/y.txt /a/z.txt   (same dir, diff name)
          | Both       -- ^ mv /a/y.txt /b/z.txt   (diff dir, diff name)


{- | For 'mv' ~ Delete + Copy
-}
tRenameFile ∷ DB → FilePath → FilePath → IO Errno
tRenameFile db fromPath toPath = do
  dbg $ "RenameFile: " ++ fromPath ++ " " ++ toPath
  maybeEntity ← fileEntityFromPath db fromPath
  case maybeEntity of
    Nothing →
      return eNOENT

    Just (FileEntity fileId _) →
      f db fileId fromPath toPath
      where f = case moveType fromPath toPath of
              Retagging →
                retagging
              Renaming →
                renaming
              Both →
                bothChanges


----------------

type MoveFn = DB → FileId → FilePath → FilePath → IO Errno


bothChanges ∷ MoveFn
bothChanges db fileId fromPath toPath = do
  dbg "  both:"
  retagging db fileId fromPath toPath
  renaming  db fileId fromPath toPath


renaming ∷ MoveFn
renaming db fileId fromPath toPath = do
  dbg "  - renaming"
  let (_, maybeToName) = parseFilePath toPath
  case maybeToName of
    Nothing →
      return eNOENT  -- What would a better response code be?

    Just toName → do
      renameFile db fileId toName
      return eOK


retagging ∷ MoveFn
retagging db fileId fromPath toPath = do
  dbg "  - retagging"

  let (fromTagNames, _) = parseFilePath fromPath
      (toTagNames,   _) = parseFilePath toPath

  -- 1. For last tagName of fromPath (if any), rm FileTag.
  if null fromTagNames
    then return ()
    else rmFileTag db fileId (last fromTagNames)

  -- 2. For all tags in toPath absent from fromPath, ensure new FileTags.
  -- TODO: determine which toTagNames are already in fromTagNames (but first).
  tagFile db fileId toTagNames -- \\ fromTagNames)
  return eOK


---------------------


moveType ∷ FilePath → FilePath → Move
moveType fromPath toPath =
  case (sameTags, sameName) of
    (False, True) →
      Retagging
    (True,  False) →
      Renaming
    (False, False) →
      Both
    _ → error "paths shouldn't be the same"
  where
    (fromTags, maybeFromName) = parseFilePath fromPath
    (toTags,   maybeToName)   = parseFilePath toPath
    sameTags = fromTags == toTags
    sameName = maybeFromName == maybeToName

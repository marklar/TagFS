{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Dir.Base
  ( openDir
  , removeDir
  ) where


import           System.Fuse

import           DB.Base
import           DB.Read
import           DB.Write
import           Debug                   (dbg)
import           Parse                   (parseDirPath)


{- | If filePath maps to a dir: eOK. Else: eNOTDIR.
-}
openDir ∷ DB → FilePath → IO Errno
openDir db filePath = do
  dbg $ "Opening dir " ++ filePath

  if filePath == "/"
    then return eOK
    else do fileEntities ← fileEntitiesFromTags db (parseDirPath filePath)
            if null fileEntities
              then return eNOTDIR
              else do dbg "  Found dir"
                      return eOK



{- | What should this mean?

E.g.
$ rmdir /foo/bar

Rm FileTag 'bar' from files w/ both tags 'foo' and 'bar'.

If Tag 'bar' no longer has any FileTags, then delete the Tag.
-}
removeDir ∷ DB → FilePath → IO Errno
removeDir db filePath = do
  dbg $ "RemoveDir: " ++ filePath

  let tagNames = parseDirPath filePath
  -- Find all files (if any) with that (complete) tagSet.
  fileEntities ← fileEntitiesFromTags db tagNames
  if null fileEntities
    then return eNOTDIR
    else do let tagName = last tagNames
            -- Untag last tag from each.
            mapM_ (\f → rmFileTag db (fileId f) tagName) fileEntities
            -- Does tagName have any associated FileTags?
            -- If not, then kill the Tag.
            hasFile ← tagHasFile db tagName
            if hasFile
              then return ()
              else rmTag db tagName
            return eOK

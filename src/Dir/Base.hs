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


{- | If filePath maps to a dir: eOK. Else: eNOENT.
-}
openDir ∷ DB → FilePath → IO Errno
openDir db filePath = do
  dbg $ "Opening dir " ++ filePath

  if filePath == "/"
    then return eOK
    else do fileEntities ← fileEntitiesFromTags db (parseDirPath filePath)
            if null fileEntities
              then return eNOENT
              else do dbg "  Found dir"
                      return eOK



{- | What should this mean?

E.g.
$ rmdir /foo/bar
Delete tag 'bar' (but not 'foo')?
Delete tags 'foo' and 'bar' both?

Answer: Rm FileTag 'bar' from files w/ both tags.
-}
removeDir ∷ DB → FilePath → IO Errno
removeDir db filePath = do
  dbg $ "RemoveDir: " ++ filePath

  let tagNames = parseDirPath filePath
  -- Find all files (if any) with that (complete) tagSet.
  fileEntities ← fileEntitiesFromTags db tagNames
  if null fileEntities
    then return eNOENT
    else do let tagName = last tagNames
            -- Untag last tag from each.
            mapM_ (\f → rmFileTag db (fileId f) tagName) fileEntities
            return eOK

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Entry where

import           Data.Maybe              (isJust)
import           Control.Monad
import           System.Fuse             (getFuseContext)

import           DB.Find                 (fileFromName, tagFromName)
import           DB.Model
import           Stat                    (dirStat, fileStat)
import           Types


findEntryByName ∷ DB → String → IO (Maybe Entry)
findEntryByName db name = do
  maybeFile ← findFileByName db name
  if isJust maybeFile
    then return maybeFile
    else findTagByName db name


findFileByName ∷ DB → String → IO (Maybe Entry)
findFileByName db name =
  fileFromName db name >>= entityToEntry


findTagByName ∷ DB → String → IO (Maybe Entry)
findTagByName db name =
  tagFromName db name >>= entityToEntry


entityToEntry ∷ Maybe Entity → IO (Maybe Entry)
entityToEntry maybeEntity = do
  ctx ← getFuseContext
  case maybeEntity of
    Just (FileEntity _ (File _ contents)) →
      return $ Just $ FileEntry (fileStat ctx) contents
    Just (TagEntity _ (Tag _)) →
      return $ Just $ DirEntry (dirStat ctx)
    Nothing →
      return Nothing

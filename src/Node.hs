{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}


{- | These functions take only the NAME, not the whole path.
-}
module Node
  ( nodeNamed
  , fileNodeNamed
  , tagNodeNamed
  ) where


import           Data.Maybe            (isJust)
import           System.Fuse           (getFuseContext)

import           DB.Base
import           DB.Read               (fileEntityNamed, tagEntityNamed)
import           Stat.Base             (dirStat, contentsFileStat)
import           Types


nodeNamed ∷ DB → String → IO (Maybe Node)
nodeNamed db name = do
  maybeFile ← fileNodeNamed db name
  if isJust maybeFile
    then return maybeFile
    else tagNodeNamed db name


fileNodeNamed ∷ DB → String → IO (Maybe Node)
fileNodeNamed db name =
  fileEntityNamed db name >>= entityToNode


tagNodeNamed ∷ DB → String → IO (Maybe Node)
tagNodeNamed db name =
  tagEntityNamed db name >>= entityToNode


----------------


entityToNode ∷ Maybe Entity → IO (Maybe Node)
entityToNode maybeEntity = do
  ctx ← getFuseContext
  case maybeEntity of
    Just (FileEntity _ (File _ contents)) →
      return $ Just $ FileNode (contentsFileStat ctx contents) contents
    Just (TagEntity _ (Tag _)) →
      return $ Just $ DirNode (dirStat ctx)
    Nothing →
      return Nothing

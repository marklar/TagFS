{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Stat.File where

import           System.Fuse

import           Debug                   (dbg)
import           DB.Model
import           DB.Find                 (fileEntitiesFromTags)
import           File                    (fileEntityFromPath)  -- TODO: mv to utils
import           Parse                   (parseDirPath)
import           Stat.Base               (dirStat, fileStat)


{- | getattr: info about inode (number, owner, last access)
   Should work for either Files or Dirs.
   (Perhaps 'FileStat' should be renamed to 'NodeStat'?)
-}
getFileStat ∷ DB → FilePath → IO (Either Errno FileStat)
getFileStat db filePath = do
  dbg $ "GetFileStat: " ++ filePath
  ctx ← getFuseContext

  case filePath of

    -- What the hell is this?
    "/._." →
      return $ Left eNOENT

    -- Root dir: show all files & tags.
    "/" → do
      return $ Right (dirStat ctx)

    -- File or Dir?
    _ → do
      maybeFileEntity ← fileEntityFromPath db filePath
      case maybeFileEntity of

        -- TODO: store stat info w/ file & return it here.
        Just (FileEntity _ _) →
          return $ Right (fileStat ctx)

        Nothing → do
          -- error "need a function that recursively looks up stats"
          fileEntities ← fileEntitiesFromTags db (parseDirPath filePath)
          if null fileEntities
            then return $ Left eNOENT
            else return $ Right (dirStat ctx)


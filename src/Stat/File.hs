{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Stat.File where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B
import           System.Fuse

import           Debug                   (dbg)
import           DB.Base
import           DB.Read                 ( fileEntitiesFromTags
                                         , fileEntityFromPath )
import           Parse                   (parseDirPath)
import           Stat.Base               (dirStat, contentsFileStat)


{- | getattr: info about inode (number, owner, last access)
   Should work for either Files or Dirs.
   (Perhaps 'FileStat' should be renamed to 'NodeStat'?)
-}
getFileStat ∷ DB → FilePath → IO (Either Errno FileStat)
getFileStat db filePath = do
  dbg $ "GetFileStat: " ++ filePath
  ctx ← getFuseContext

  case filePath of
    "/._." →
      return $ Left eNOENT

    "/" → do
      return $ Right (dirStat ctx)

    -- File or Dir?
    _ → do

      -- TODO:
      -- If filePath has a fileName with a version number in it,
      -- then we want to cheat here and return fStat of whatever
      -- version of that file actually is in this directory.

      maybeFileEntity ← fileEntityFromPath db filePath
      case maybeFileEntity of
        -- TODO: store stat info w/ file.
        Just (FileEntity _ (File _ contents)) →
          return $ Right (contentsFileStat ctx contents)

        Nothing → do
          fileEntities ← fileEntitiesFromTags db (parseDirPath filePath)
          if null fileEntities
            then return $ Left eNOENT
            else return $ Right (dirStat ctx)

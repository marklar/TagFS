{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Dir.Read
  ( readDir
  ) where

import           Data.List               (union, (\\))
import           Data.Maybe              (catMaybes)
import           System.Fuse
import           System.Posix.Types

import           DB.Find
import           DB.Insert               -- (rmFileTag)   -- FIXME
import           DB.Model
import           Debug                   (dbg)
import           Parse                   (parseDirPath)
import           Stat.Base               (dirStat, fileStat)


{- | Entire contents of dir. Not just files - also sub-dirs (tags).
-}
readDir ∷ DB → FilePath → IO (Either Errno [(FilePath, FileStat)])
readDir db filePath = do
  dbg $ "ReadDir: " ++ filePath

  ctx ← getFuseContext
  let baseDirs = [ (".",  dirStat ctx)
                 , ("..", dirStat ctx)
                 ]

  if filePath == "/"
    -- Include ALL TAGS and ALL FILES.
    then do fileEntities ← allFileEntities db
            files ← fileNamesWithStats db fileEntities
            tagEntities ← allTagEntities db
            tags ← tagNamesWithStats db tagEntities
            return $ Right (baseDirs ++ tags ++ files)

    -- Find all files (if any) with that (complete) tagSet.
    else do let tagNames = parseDirPath filePath
            fileEntities ← fileEntitiesFromTags db tagNames
            dbg $ "  num files: " ++ (show $ length fileEntities)
            if null fileEntities
              then return $ Left eNOENT
              else do files ← fileNamesWithStats db fileEntities
                      -- TODO: delete 'dummy' from files
                      tagEntities ← allTagsForFilesExcept db fileEntities tagNames
                      -- filter out the ones whose name match here
                      tags ← tagNamesWithStats db tagEntities
                      return $ Right (baseDirs ++ files ++ tags)


allTagsForFilesExcept ∷ DB → [Entity] → [TagName] → IO [Entity]
allTagsForFilesExcept db fileEntities tagNames = do
  let fileNames = map (\(FileEntity _ (File name _)) → name) fileEntities
  tagNameLists ← mapM (tagsForFileName db) fileNames
  let allTagNames = foldr1 union tagNameLists
  maybeEntities ← mapM (tagEntityNamed db) (allTagNames \\ tagNames)
  return $ catMaybes maybeEntities


tagNamesWithStats ∷ DB → [Entity] → IO [(FileName, FileStat)]
tagNamesWithStats db tagEntities = do
  ctx ← getFuseContext
  return $ flip map tagEntities (\(TagEntity _ (Tag name)) →
                                   (name, dirStat ctx))


fileNamesWithStats ∷ DB → [Entity] → IO [(FileName, FileStat)]
fileNamesWithStats db fileEntities = do
  ctx ← getFuseContext
  return $ flip map fileEntities (\(FileEntity _ (File name _)) →
                                    (name, fileStat ctx))


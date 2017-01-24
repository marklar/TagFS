{-# LANGUAGE UnicodeSyntax              #-}

module DB.Read
  ( allFileEntities
  , allTagEntities
  , fileEntitiesFromTags
  , fileEntityFromTagsAndName
  , fileEntityFromPath
  , fileEntityNamed
  , tagEntityNamed
  , tagsForFileName
  ) where


import           Data.Maybe               (catMaybes)
import           Data.List                (intersect)
import           Database.HDBC            (SqlValue, fromSql, toSql)

import           Debug
import           DB.Model
import           DB.Row                   (findRowById, findRowByName)
import           Parse                    (parseFilePath)


fileEntityFromPath ∷ DB → FilePath → IO (Maybe Entity)
fileEntityFromPath db filePath = do
  let (tagNames, maybeFileName) = parseFilePath filePath
  case maybeFileName of
    Nothing →
      return Nothing
    Just fileName → do
      fileEntityFromTagsAndName db tagNames fileName


allFileEntities ∷ DB → IO [Entity]
allFileEntities db = do
  let sql = "SELECT * FROM files"
  valLists ← queryWithClone db sql []
  return $ map rowToEntity valLists
  where
    rowToEntity ∷ [SqlValue] → Entity
    rowToEntity [id, name, contents] =
      FileEntity (fromSql id) (File (fromSql name) (fromSql contents))


allTagEntities db = do
  let sql = "SELECT * FROM tags"
  valLists ← queryWithClone db sql []
  return $ map rowToEntity valLists
  where
    rowToEntity ∷ [SqlValue] → Entity
    rowToEntity [id, name] =
      TagEntity (fromSql id) (Tag (fromSql name))


-----------------------


{- | Works with non-null [TagName]. For [], we want all files.
-}
fileEntitiesFromTags ∷ DB → [TagName] → IO [Entity]
fileEntitiesFromTags conn tagNames = do
  fileIdLists ← mapM (fileIdsForTag conn) tagNames
  maybeEntities ← mapM (fileEntityById conn) (foldr1 intersect fileIdLists)
  return $ catMaybes maybeEntities


{- | Search DB for file w/ name and all its tag names.
-}
fileEntityFromTagsAndName ∷ DB
                          → [TagName]    -- ^ Tag names
                          → FileName     -- ^ File name
                          → IO (Maybe Entity)
fileEntityFromTagsAndName conn tagNames name = do
  if name == "._."
    then return Nothing
    else do allTagNames ← tagsForFileName conn name
            -- dbg $ "  Find.fileEntityFromTagsAndName, tagNames: " ++ show tagNames
            if all (\n → elem n allTagNames) tagNames
              then fileEntityNamed conn name
              else return Nothing


-- Find FileEntity. If not ∃, "<fileName>: No such file or directory"
fileEntityNamed ∷ DB → FileName → IO (Maybe Entity)
fileEntityNamed conn name = do
  maybeRow ← findRowByName conn "files" name
  case maybeRow of
    Nothing →
      return Nothing
    Just [id, _, contents] → do
      return $ Just (FileEntity (fromSql id)
                      (File name (fromSql contents)))


tagEntityNamed ∷ DB → TagName → IO (Maybe Entity)
tagEntityNamed conn name = do
  maybeRow ← findRowByName conn "tags" name
  case maybeRow of
    Nothing →
      return Nothing
    Just [id, _] →
      return . Just $ TagEntity (fromSql id) $ Tag name


---------------------
---------------------


fileIdsForTag ∷ DB → TagName → IO [FileId]
fileIdsForTag conn tagName = do
  let sql = "SELECT      files_tags.file_id " ++
            "FROM        files_tags " ++
            "INNER JOIN  tags " ++
            "ON          tags.name = ? " ++
            "AND         files_tags.tag_id = tags.id"
  ids ← queryWithClone conn sql [toSql tagName]
  return $ map fromSql (concat ids)


fileEntityById ∷ DB → FileId → IO (Maybe Entity)
fileEntityById conn fileId = do
  maybeRow ← findRowById conn "files" fileId
  case maybeRow of
    Nothing →
      return Nothing
    Just [_, name, contents] →
      return $ Just (FileEntity fileId
                      (File (fromSql name) (fromSql contents)))


{- | Helper fn. Sometimes when you have the name of a file, you want to
   know how it's tagged.
-}
tagsForFileName ∷ DB → FileName → IO [TagName]
tagsForFileName conn fileName = do
  maybeFileEntity ← fileEntityNamed conn fileName
  case maybeFileEntity of
    Nothing →
      return []
    Just (FileEntity fileId _) → do
      let sql = "SELECT     ts.name " ++
                "FROM       tags ts " ++
                "INNER JOIN files_tags fts " ++
                "ON         ts.id = fts.tag_id " ++
                "AND        fts.file_id = ?"
      r ← queryWithClone conn sql [toSql fileId]
      return $ map (fromSql . head) r

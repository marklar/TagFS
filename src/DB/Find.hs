{-# LANGUAGE UnicodeSyntax              #-}

module DB.Find where

import qualified Data.ByteString.Char8   as B

import           Control.Monad            (liftM)
import           Data.Maybe               (isJust, catMaybes)
import           Data.List                (intersect)
import           Database.HDBC            (SqlValue, fromSql, toSql)
import           Database.HDBC.Sqlite3    (Connection)

import           Debug
import           DB.Model


filesFromTags ∷ Connection → [TagName] → IO [Entity]
filesFromTags conn tagNames = do
  fileIdLists ← mapM (fileIdsForTag conn) tagNames
  let fileIds = foldr1 (\ids acc → intersect ids acc) fileIdLists
  maybeEntities ← mapM (fileEntityById conn) fileIds
  return $ catMaybes maybeEntities


fileIdsForTag ∷ Connection → TagName → IO [FileId]
fileIdsForTag conn tagName = do
  let sql = "SELECT      files_tags.file_id " ++
            "FROM        files_tags " ++
            "INNER JOIN  tags " ++
            "ON          tags.name = ? " ++
            "AND         files_tags.tag_id = tags.id"
  ids ← queryWithClone conn sql [toSql tagName]
  return $ map fromSql (concat ids)


-----------------------


fileEntityById ∷ Connection → FileId → IO (Maybe Entity)
fileEntityById conn fileId = do
  maybeRow ← findRowById conn "files" fileId
  case maybeRow of
    Nothing →
      return Nothing
    Just [_, name, contents] →
      return $ Just (FileEntity fileId
                      (File (fromSql name) (fromSql contents)))



{- | Search DB for file w/ name and all its tag names.
-}
fileEntityFromTagsAndName ∷ Connection
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


{- | Helper fn. Sometimes when you have the name of a file, you want to
   know how it's tagged.
-}
tagsForFileName ∷ Connection → FileName → IO [TagName]
tagsForFileName conn fileName = do
  maybeFileEntity ← fileEntityNamed conn fileName
  case maybeFileEntity of
    Nothing →
      return []
    Just (FileEntity fileId _) → do
      dbg $ "  inner join"
      let sql = "SELECT     ts.name " ++
                "FROM       tags ts " ++
                "INNER JOIN files_tags fts " ++
                "ON         ts.id = fts.tag_id " ++
                "AND        fts.file_id = ?"
      r ← queryWithClone conn sql [toSql fileId]
      return $ map (fromSql . head) r


-------------------------

-- Find FileEntity. If not ∃, "<fileName>: No such file or directory"
fileEntityNamed ∷ Connection → FileName → IO (Maybe Entity)
fileEntityNamed conn name = do
  maybeRow ← findRowByName conn "files" name
  case maybeRow of
    Nothing →
      return Nothing
    Just [id, _, contents] → do
      return $ Just (FileEntity (fromSql id)
                      (File name (fromSql contents)))


tagEntityNamed ∷ Connection → TagName → IO (Maybe Entity)
tagEntityNamed conn name = do
  maybeRow ← findRowByName conn "tags" name
  case maybeRow of
    Nothing →
      return Nothing
    Just [id, _] →
      return . Just $ TagEntity (fromSql id) $ Tag name


-----------------------


fileExists ∷ Connection → FileName → IO Bool
fileExists = rowByNameExists "files"


tagExists ∷ Connection → TagName → IO Bool
tagExists = rowByNameExists "tags"


rowByNameExists ∷ String → Connection → String → IO Bool
rowByNameExists tableName conn nameVal =
  liftM isJust $ findRowByName conn tableName nameVal


----------------


findRowByName ∷ Connection
              → String   -- ^ table name
              → String   -- ^ name value
              → IO (Maybe [SqlValue])
findRowByName conn tableName name =
  findRowByVal conn tableName "name" (toSql name)


findRowById ∷ Connection
            → String   -- ^ table name
            → Integer  -- ^ row ID
            → IO (Maybe [SqlValue])
findRowById conn tableName id =
  findRowByVal conn tableName "id" (toSql id)
  

findRowByVal ∷ Connection
             → String   -- ^ table name
             → String   -- ^ column name
             → SqlValue
             → IO (Maybe [SqlValue])
findRowByVal conn tableName colName sqlVal = do
  let sql = "SELECT * " ++
            "FROM   " ++ tableName ++ " " ++
            "WHERE  " ++ tableName ++ "." ++ colName ++ " = ? " ++
            "LIMIT  1"
  r ← queryWithClone conn sql [sqlVal]
  case r of
    [] → do
      return Nothing
    vals : _ → do
      return $ Just vals

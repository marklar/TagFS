{-# LANGUAGE UnicodeSyntax              #-}

module DB.Find where

import           Control.Monad            (liftM)
import           Data.Maybe               (isJust, catMaybes)
import           Data.List                (intercalate)
import           Database.HDBC            (SqlValue, quickQuery', fromSql, toSql)
import           Database.HDBC.Sqlite3    (Connection)

import           DB.Model


filesFromTags ∷ Connection → [TagName] → IO [Entity]
filesFromTags conn tagNames = do
  r ← quickQuery' conn ( "SELECT      ft.file_id, COUNT(t.tag_id) " ++
                         "FROM        files_tags ft " ++
                         "INNER JOIN  tags t " ++
                         "ON          t.name in (?) " ++
                         "AND         t.id = ft.tag_id "
                       ) [toSql $ intercalate "," tagNames]
  let fileIds =
        map (fromSql . head) $
        filter (\[_, numTags] → (fromSql numTags ∷ Int) == 3) r
  maybeEntities ← sequence $ map (fileEntityById conn) fileIds
  return $ catMaybes maybeEntities


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
  allTagNames ← tagsForFileName conn name
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
      r ← quickQuery' conn ( "SELECT     ts.name " ++
                             "FROM       tags ts " ++
                             "INNER JOIN files_tags fts " ++
                             "ON         ts.id = fts.tag_id " ++
                             "AND        fts.file_id = ?"
                           ) [toSql fileId]
      return $ map (fromSql . head) r


-------------------------

-- Find FileEntity. If not ∃, "<fileName>: No such file or directory"
fileEntityNamed ∷ Connection → FileName → IO (Maybe Entity)
fileEntityNamed conn name = do
  maybeRow ← findRowByName conn "files" name
  case maybeRow of
    Nothing →
      return Nothing
    Just [id, _, contents] →
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
findRowByName conn tableName name = do
  findRowByVal conn tableName "name" (toSql name)


findRowById ∷ Connection
            → String   -- ^ table name
            → Integer  -- ^ row ID
            → IO (Maybe [SqlValue])
findRowById conn tableName id = do
  findRowByVal conn tableName "id" (toSql id)


findRowByVal ∷ Connection
             → String   -- ^ table name
             → String   -- ^ column name
             → SqlValue
             → IO (Maybe [SqlValue])
findRowByVal conn tableName colName val = do
  r ← quickQuery' conn ( "SELECT * " ++
                         "FROM   ? " ++
                         "WHERE  ? = ? " ++
                         "LIMIT  1" )
      [toSql tableName, toSql colName, val]
  case r of
    [] →
      return Nothing
    vals : _ →
      return $ Just vals

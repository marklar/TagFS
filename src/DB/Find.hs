{-# LANGUAGE UnicodeSyntax              #-}

module DB.Find where

import           Control.Monad            (liftM)
import           Data.Maybe               (isJust)
import           Database.HDBC            (SqlValue, quickQuery', fromSql, toSql)
import           Database.HDBC.Sqlite3    (Connection)

import           DB.Model


{- | Search DB for file w/ name and all its tag names.
-}
fileEntityFromTagsAndName ∷ Connection
                          → [String]    -- ^ Tag names
                          → FileName    -- ^ File name
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


findRowByName ∷ Connection → String → String → IO (Maybe [SqlValue])
findRowByName conn tableName name = do
  r ← quickQuery' conn ("SELECT * " ++
                        "FROM   ? " ++
                        "WHERE  name = ? " ++
                        "LIMIT  1")
      [toSql tableName, toSql name]
  case r of
    [] → return Nothing
    vals : _ →
      return $ Just vals


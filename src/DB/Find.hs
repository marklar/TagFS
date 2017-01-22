{-# LANGUAGE UnicodeSyntax              #-}

module DB.Find where

import           Control.Monad            (liftM)
import           Data.Maybe               (isJust)
import           Database.HDBC            (SqlValue, quickQuery', fromSql, toSql)
import           Database.HDBC.Sqlite3    (Connection)

import           DB.Model


fileNamesFromTagId ∷ Connection → TagId → IO [FileName]
fileNamesFromTagId conn tagId = do
  r ← quickQuery' conn ("SELECT files.name " ++
                        "FROM   files, files_tags " ++
                        "WHERE  files_tags.tag_id = ? " ++
                        "AND    files.id = files_tags.file_id") [toSql tagId]
  return (map (\(res:_) → fromSql res) r)


-- Find FileEntity. If not ∃, "<fileName>: No such file or directory"
fileFromName ∷ Connection → FileName → IO (Maybe Entity)
fileFromName conn name = do
  maybeRow ← findRowByName conn "files" name
  case maybeRow of
    Nothing →
      return Nothing
    Just [id, _, contents] →
      return $ Just (FileEntity (fromSql id)
                      (File name (fromSql contents)))


tagFromName ∷ Connection → TagName → IO (Maybe Entity)
tagFromName conn name = do
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


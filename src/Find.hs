{-# LANGUAGE UnicodeSyntax              #-}

module Find where

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Model


fileNamesFromTagId ∷ Connection → TagId → IO [FileName]
fileNamesFromTagId conn tagId = do
  r ← quickQuery' conn ("SELECT files.name " ++
                        "FROM   files, files_tags " ++
                        "WHERE  files_tags.tag_id = ? " ++
                        "AND    files.id = files_tags.file_id") [toSql tagId]
  return (map (\(res:_) → fromSql res) r)


-- Find FileEntity. If not ∃, "<fileName>: No such file or directory"
fileFromName ∷ Connection → FileName → IO (Maybe FileEntity)
fileFromName conn name = do
  r ← quickQuery' conn "SELECT * FROM files WHERE name = ? LIMIT 1" [toSql name]
  case r of
    [] → return Nothing
    [id, name, contents] : _ →
      return $ Just (FileEntity (fromSql id)
                      (File (fromSql name) (fromSql contents)))


tagFromName ∷ Connection → TagName → IO (Maybe TagEntity)
tagFromName conn tagName = do
  r ← quickQuery' conn "SELECT * FROM tags WHERE name = ? LIMIT 1" [toSql tagName]
  case r of
    [] → return Nothing
    [id, name] : _ →
      return . Just $ TagEntity (fromSql id) $ Tag (fromSql name)


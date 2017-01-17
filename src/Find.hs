{-# LANGUAGE UnicodeSyntax              #-}

module Find where

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Model


-- Find FileEntity. If not ∃, "<fileName>: No such file or directory"
fileFromName ∷ Connection → FileName → IO (Maybe FileEntity)
fileFromName conn name = do
  r ← quickQuery' conn "SELECT * from file WHERE name = ? LIMIT 1" [toSql name]
  case r of
    [] → return Nothing
    [id, name, contents] : _ →
      return $ Just (FileEntity (fromSql id)
                      (File (fromSql name) (fromSql contents)))


tagFromName ∷ Connection → TagName → IO (Maybe TagEntity)
tagFromName conn tagName = do
  r ← quickQuery' conn "SELECT * from tag WHERE name = ? LIMIT 1" [toSql tagName]
  case r of
    [] → return Nothing
    [id, name] : _ →
      return . Just $ TagEntity (fromSql id) $ Tag (fromSql name)


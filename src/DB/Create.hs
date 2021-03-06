{-# LANGUAGE UnicodeSyntax              #-}

module DB.Create where

import           Database.HDBC           (run, commit)
import           DB.Base


createDb ∷ FilePath → IO ()
createDb dbName = do
  conn ← connect dbName

  run conn ("CREATE TABLE IF NOT EXISTS tags " ++
            "(id INTEGER PRIMARY KEY," ++
            " name VARCHAR NOT NULL UNIQUE)") []

  -- length
  -- mtime, ctime, atime
  -- inode (int)
  run conn ("CREATE TABLE IF NOT EXISTS files " ++
            "(id INTEGER PRIMARY KEY," ++
            " name VARCHAR NOT NULL UNIQUE," ++
            " contents BLOB NOT NULL)") []

  run conn ("CREATE TABLE IF NOT EXISTS files_tags " ++
            "(id INTEGER PRIMARY KEY," ++
            " file_id INTEGER NOT NULL REFERENCES files," ++
            " tag_id INTEGER NOT NULL REFERENCES tags," ++
            " CONSTRAINT unique_file_tag UNIQUE (file_id, tag_id))") []

  commit conn
  disconnect conn


{-
dbStuff ∷ String → IO ()
dbStuff dbFileName = do
  createDb dbFileName
  conn ← connectSqlite3 dbFileName
  -- addSomeData conn
  -- viewData conn
  disconnect conn
-}


{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module DataStore.Create where

import           Database.HDBC
import           Database.HDBC.Sqlite3

import           DataStore.Model


createDb ∷ String → IO ()
createDb dbName = do
  conn <- connectSqlite3 dbName

  run conn ("CREATE TABLE tags " ++
            "(id INTEGER PRIMARY KEY," ++
            " name VARCHAR NOT NULL UNIQUE)") []

  -- TODO: Add attributes to Files (perhaps in 2 separate tables).
  -- file metadata
  --   absolute path
  --   length
  --   mtime, ctime, atime
  --   inode (int)
  -- file contents
  --   sha (text)
  --   data (blob)
  --   length (int)
  --   compressed (blob)
  run conn ("CREATE TABLE files " ++
            "(id INTEGER PRIMARY KEY," ++
            " name VARCHAR NOT NULL," ++
            " contents BLOB NOT NULL)") []

  run conn ("CREATE TABLE files_tags " ++
            "(id INTEGER PRIMARY KEY," ++
            " file_id INTEGER NOT NULL REFERENCES files," ++
            " tag_id INTEGER NOT NULL REFERENCES tags," ++
            " CONSTRAINT unique_file_tag UNIQUE (file_id, tag_id))") []

  commit conn
  disconnect conn

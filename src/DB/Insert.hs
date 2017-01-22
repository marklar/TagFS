{-# LANGUAGE UnicodeSyntax              #-}

module DB.Insert
  ( mkFile
  , mkTag
  , mkFileTag
  ) where


import           Database.HDBC
import           Database.HDBC.Sqlite3

import           DB.Model


mkFile ∷ Connection → File → IO ()
mkFile conn (File name contents) = do
  stmt ← prepare conn "INSERT INTO files VALUES (?, ?, ?)"
  execute stmt [SqlNull, toSql name, toSql contents]
  commit conn


mkTag ∷ Connection → Tag → IO ()
mkTag conn (Tag name) = do
  stmt ← prepare conn "INSERT INTO tags VALUES (?, ?)"
  execute stmt [SqlNull, toSql name]
  commit conn


mkFileTag ∷ Connection → FileId → TagId → IO ()
mkFileTag conn fileId tagId = do
  stmt ← prepare conn "INSERT INTO files_tags VALUES (?, ?, ?)"
  execute stmt [SqlNull, toSql fileId, toSql tagId]
  commit conn

{-# LANGUAGE UnicodeSyntax              #-}

module Insert where

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Model


mkFile ∷ Connection → File → IO ()
mkFile conn (File name contents) = do
  stmt ← prepare conn "INSERT INTO file VALUES (?, ?, ?)"
  execute stmt [SqlNull, toSql name, toSql contents]
  commit conn


mkTag ∷ Connection → Tag → IO ()
mkTag conn (Tag name) = do
  stmt ← prepare conn "INSERT INTO tag VALUES (?, ?)"
  execute stmt [SqlNull, toSql name]
  commit conn


mkFileTag ∷ Connection → FileId → TagId → IO ()
mkFileTag conn fileId tagId = do
  stmt ← prepare conn "INSERT INTO file_tag VALUES (?, ?, ?)"
  execute stmt [SqlNull, toSql fileId, toSql tagId]
  commit conn
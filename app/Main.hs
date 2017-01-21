{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           System.Fuse

import           HdbcDb
import           Model
import           FuseOps


dbFile ∷ FilePath
dbFile = "flurbl.db"


main ∷ IO ()
main = do
  -- dbStuff
  conn ← connectSqlite3 dbFile
  runFuse conn


--------------

dbStuff ∷ IO ()
dbStuff = do
  -- createDb dbFile
  conn ← connectSqlite3 dbFile
  -- addSomeData conn
  viewData conn
  disconnect conn



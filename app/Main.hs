{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           System.Fuse

import           DataStore.Create
import           DataStore.Model
import           FuseOps


dbFile ∷ FilePath
dbFile = "/Users/markwong-vanharen/Development/TagFS/flurbl.db"


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
  -- viewData conn
  disconnect conn



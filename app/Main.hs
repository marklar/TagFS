{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           HdbcDb
import           Model


main ∷ IO ()
main = do
  createDb "flurbl.db"
  conn <- connectSqlite3 "flurbl.db"
  addSomeData conn
  disconnect conn

{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           DB.Create
import           FuseOps

import DB.Find
import Debug


dbFile ∷ FilePath
dbFile = "/Users/markwong-vanharen/Development/TagFS/flurbl.db"


main ∷ IO ()
main = do
  db ← connect dbFile
  foo ← findRowByName db "files" "football.txt"
  dbg $ "foo: " ++ show foo
  runFuse db

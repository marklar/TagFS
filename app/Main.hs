{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           DB.Base
import           DB.Create
import           FuseOps


dbFile ∷ FilePath
dbFile = "/Users/markwong-vanharen/Development/TagFS/flurbl.db"


main ∷ IO ()
main = do
  -- createDb dbFile
  db ← connect dbFile
  runFuse db

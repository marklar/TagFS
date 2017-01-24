{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           DB.Base
import           FuseOps


dbFile ∷ FilePath
dbFile = "/Users/markwong-vanharen/Development/TagFS/flurbl.db"


main ∷ IO ()
main = do
  db ← connect dbFile
  runFuse db

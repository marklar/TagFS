{-# LANGUAGE UnicodeSyntax #-}
module Main where

import           System.Environment
import           DB.Base
import           DB.Create
import           FuseOps


main ∷ IO ()
main = do
  dbFile ← getEnv "TAGFS_DB"
  createDb dbFile
  db ← connect dbFile
  runFuse db

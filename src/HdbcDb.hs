{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module HdbcDb where

import           Data.ByteString
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Control.Monad.IO.Class  (liftIO)

import           Find
import           Insert
import           Model
import           Tag


addFiles ∷ Connection → IO ()
addFiles conn = do
  mkFile conn (File "football.txt" "Some amazing content about the Packers")
  mkFile conn (File "futbol.txt"   "Me's contingut sorprenent sobre el Barc,a")


addTags ∷ Connection → IO ()
addTags conn = do
  mkTag conn (Tag "sports")
  mkTag conn (Tag "barca")
  mkTag conn (Tag "packers")
  

addSomeData ∷ Connection → IO ()
addSomeData conn = do
  addFiles conn
  -- addTags conn
  multiTagFile conn "football.txt" ["sports", "packers"]
  multiTagFile conn "futbol.txt"   ["sports", "barca"]

  let getFileTagsSql = "SELECT * from file_tag" -- , ORDER BY id, asc"
  r ← quickQuery' conn getFileTagsSql []
  liftIO $ Prelude.putStrLn "file_tags:"
  liftIO $ print r

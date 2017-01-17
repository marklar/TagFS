{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module HdbcDb where

import           Data.ByteString
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Control.Monad.IO.Class  (liftIO)

import           Model


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


-----------------------
  -- -- get File IDs
  -- let getFileSql = "SELECT * from file WHERE name = ? LIMIT 1"
  -- [[footballIdSql : _], [futbolIdSql : _]] ←
  --   mapM (\s → quickQuery' conn getFileSql [toSql s]) (["football.txt", "futbol.txt"] ∷ [String])
  -- let [footballId, futbolId] =
  --       fmap (\v → (fromSql v) ∷ Integer) [footballIdSql, futbolIdSql]

  -- -- get Tag IDs
  -- let getTagSql = "SELECT * from tag WHERE name = ? LIMIT 1"
  -- [ [(sportsIdSql:_)], [(barcaIdSql:_)], [(packersIdSql:_)] ] ←
  --   mapM (\s → quickQuery' conn getTagSql [toSql s]) (["sports", "barca", "packers"] ∷ [String])
  -- let [sportsId, barcaId, packersId] =
  --       fmap (\s → (fromSql s) ∷ Integer) [ sportsIdSql
  --                                         , barcaIdSql
  --                                         , packersIdSql
  --                                         ]

  -- -- FileTags
  -- fileTagStmt ← prepare conn "INSERT INTO file_tag VALUES (?, ?, ?)"
  -- mapM_ (\(fileId, tagId) → execute fileTagStmt [SqlNull, toSql fileId, toSql tagId])
  --   [ (footballId, sportsId)
  --   , (footballId, packersId)
  --   , (futbolId, sportsId)
  --   , (futbolId, barcaId)
  --   ]
  -- commit conn


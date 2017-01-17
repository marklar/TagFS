{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module Model where

import           Data.ByteString
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Control.Monad.IO.Class  (liftIO)

type FileName = String
type TagName = String
type FileId = Integer
type TagId = Integer

data File = File { fileName ∷ FileName
                 , fileContents ∷ ByteString
                 }
data FileEntity = FileEntity { fileId ∷ Integer
                             , file ∷ File
                             }

data Tag = Tag TagName
data TagEntity = TagEntity { tagId ∷ Integer
                           , tag ∷ Tag
                           }

-- unused?
data FileTag = FileTag { fId ∷ FileId
                       , tId ∷ TagId
                       }                 

createDb ∷ String → IO ()
createDb dbName = do
  conn <- connectSqlite3 dbName
  run conn ("CREATE TABLE tag " ++
            "(id INTEGER PRIMARY KEY," ++
            " name VARCHAR NOT NULL)") []
  run conn ("CREATE TABLE file " ++
            "(id INTEGER PRIMARY KEY," ++
            " name VARCHAR NOT NULL," ++
            " contents BLOB NOT NULL)") []
  run conn ("CREATE TABLE file_tag " ++
            "(id INTEGER PRIMARY KEY," ++
            " file_id INTEGER NOT NULL REFERENCES file," ++
            " tag_id INTEGER NOT NULL REFERENCES tag," ++
            " CONSTRAINT unique_file_tag UNIQUE (file_id, tag_id))") []
  commit conn
  disconnect conn

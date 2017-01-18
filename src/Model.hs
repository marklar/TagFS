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
            deriving (Show)
data FileEntity = FileEntity { fileId ∷ Integer
                             , file ∷ File
                             }
                deriving (Show)

data Tag = Tag TagName
         deriving (Show)

data TagEntity = TagEntity { tagId ∷ Integer
                           , tag ∷ Tag
                           }
               deriving (Show)

-- unused?
data FileTag = FileTag { fId ∷ FileId
                       , tId ∷ TagId
                       }                 
             deriving (Show)

createDb ∷ String → IO ()
createDb dbName = do
  conn <- connectSqlite3 dbName
  run conn ("CREATE TABLE tags " ++
            "(id INTEGER PRIMARY KEY," ++
            " name VARCHAR NOT NULL)") []
  run conn ("CREATE TABLE files " ++
            "(id INTEGER PRIMARY KEY," ++
            " name VARCHAR NOT NULL," ++
            " contents BLOB NOT NULL)") []
  run conn ("CREATE TABLE files_tags " ++
            "(id INTEGER PRIMARY KEY," ++
            " file_id INTEGER NOT NULL REFERENCES files," ++
            " tag_id INTEGER NOT NULL REFERENCES tags," ++
            " CONSTRAINT unique_file_tag UNIQUE (file_id, tag_id))") []
  commit conn
  disconnect conn

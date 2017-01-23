{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module DB.Model where

import           Data.ByteString
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Control.Monad.IO.Class  (liftIO)


type DB = Connection
type FileName = String
type TagName = String
type FileId = Integer
type TagId = Integer


data File = File { fileName ∷ FileName
                 , fileContents ∷ ByteString
                 }
            deriving (Show)


data Tag = Tag TagName
         deriving (Show)


data Entity
  = TagEntity { tagId ∷ Integer
              , tag ∷ Tag
              }
  | FileEntity { fileId ∷ Integer
               , file ∷ File
               }
  deriving (Show)


-- unused?
data FileTag = FileTag { fId ∷ FileId
                       , tId ∷ TagId
                       }                 
             deriving (Show)


connect ∷ FilePath → IO DB
connect = connectSqlite3


clone ∷ DB → IO DB
clone = Database.HDBC.clone


disconnect ∷ DB → IO ()
disconnect = Database.HDBC.disconnect


withClone ∷ DB → (DB → IO α) → IO α
withClone db f = do
  db' ← DB.Model.clone db
  r ← f db'
  DB.Model.disconnect db'
  return r


execWithClone ∷ DB → String → [SqlValue] → IO ()
execWithClone db sql args =
  withClone db (\c → do
                   stmt ← prepare c sql
                   execute stmt args
                   commit c)


queryWithClone ∷ DB → String → [SqlValue] → IO [[SqlValue]]
queryWithClone db sql args =
  withClone db (\c → quickQuery' c sql args)

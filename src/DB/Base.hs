{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE UnicodeSyntax              #-}

module DB.Base where

import           Data.ByteString
import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Control.Monad.IO.Class  (liftIO)
import Debug

type DB = Connection
type FileName = String
type TagName = String
type FileId = Integer
type TagId = Integer


data Entity
  = TagEntity { tagId ∷ Integer
              , tag ∷ Tag
              }
  | FileEntity { fileId ∷ Integer
               , file ∷ File
               }
  deriving (Show)


data File = File { fileName ∷ FileName
                 , fileContents ∷ ByteString
                 }
            deriving (Show)


data Tag = Tag TagName
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


execWithClone ∷ DB → String → [SqlValue] → IO ()
execWithClone db sql args =
  withClone db (\c → do
                   stmt ← prepare c sql
                   execute stmt args
                   commit c)


queryWithClone ∷ DB → String → [SqlValue] → IO [[SqlValue]]
queryWithClone db sql args =
  withClone db (\c → quickQuery' c sql args)


withClone ∷ DB → (DB → IO α) → IO α
withClone db f = do
  db' ← DB.Base.clone db
  r ← catchSql (f db')
        (\e → do dbg $ "ERROR: " ++ show e; f db')
  DB.Base.disconnect db'
  return r

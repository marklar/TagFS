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



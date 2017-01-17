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


multiTagFile ∷ Connection → FileName → [TagName] → IO ()
multiTagFile conn fileName tagNames = do
  mapM_ (tagFile conn fileName) tagNames


tagFile ∷ Connection → FileName → TagName → IO ()
tagFile conn fileName tagName = do
  -- Find FileEntity. If ~∃, "<fileName>: No such file or directory"
  maybeFileEntity ← fileFromName conn fileName
  case maybeFileEntity of
    Nothing →
      -- FIXME: Return error.
      return ()
    Just fileEntity → do
      -- Find or create TagEntity
      maybeTagEntity ← tagFromName conn tagName
      case maybeTagEntity of
        Nothing → do
          mkTag conn (Tag tagName)
          createFileTag conn fileEntity tagName
        Just tagEntity →
          findOrCreateFileTag conn fileEntity tagEntity

------------------

-- FIXME: What if FileTagEntity already exists?
findOrCreateFileTag ∷ Connection -> FileEntity → TagEntity → IO ()
findOrCreateFileTag conn (FileEntity fileId _) (TagEntity tagId _) =
  mkFileTag conn fileId tagId
  

createFileTag ∷ Connection → FileEntity → TagName → IO ()
createFileTag conn (FileEntity fileId _) tagName = do
  -- It *should* exist here.
  maybeTagEntity ← tagFromName conn tagName
  case maybeTagEntity of
    Nothing →
      -- FIXME: Should be error.
      return ()
    Just (TagEntity tagId (Tag name)) →
      mkFileTag conn fileId tagId

------------------

-- Find FileEntity. If not ∃, "<fileName>: No such file or directory"
fileFromName ∷ Connection → FileName → IO (Maybe FileEntity)
fileFromName conn name = do
  r ← quickQuery' conn "SELECT * from file WHERE name = ? LIMIT 1" [toSql name]
  case r of
    [] → return Nothing
    [id, name, contents] : _ →
      return $ Just (FileEntity (fromSql id)
                      (File (fromSql name) (fromSql contents)))


tagFromName ∷ Connection → TagName → IO (Maybe TagEntity)
tagFromName conn tagName = do
  r ← quickQuery' conn "SELECT * from tag WHERE name = ? LIMIT 1" [toSql tagName]
  case r of
    [] → return Nothing
    [id, name] : _ →
      return . Just $ TagEntity (fromSql id) $ Tag (fromSql name)

------------------

mkFile ∷ Connection → File → IO ()
mkFile conn (File name contents) = do
  stmt ← prepare conn "INSERT INTO file VALUES (?, ?, ?)"
  execute stmt [SqlNull, toSql name, toSql contents]
  commit conn


mkTag ∷ Connection → Tag → IO ()
mkTag conn (Tag name) = do
  stmt ← prepare conn "INSERT INTO tag VALUES (?, ?)"
  execute stmt [SqlNull, toSql name]
  commit conn


mkFileTag ∷ Connection → FileId → TagId → IO ()
mkFileTag conn fileId tagId = do
  stmt ← prepare conn "INSERT INTO file_tag VALUES (?, ?, ?)"
  execute stmt [SqlNull, toSql fileId, toSql tagId]
  commit conn

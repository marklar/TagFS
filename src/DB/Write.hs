{-# LANGUAGE UnicodeSyntax              #-}

module DB.Write
  ( ensureFileTag
  , mkFile
  , mkFileTag
  , mkTag
  , renameFile
  , rmFile
  , rmFileTag
  , rmTag
  , updateContents
  ) where


import           Database.HDBC

import           DB.Base
import           Debug


updateContents ∷ DB → File → IO ()
updateContents conn (File name contents) = do
  let sql = "UPDATE files " ++
            "SET    contents = ? " ++
            "WHERE  name = ?"
      args = [toSql contents, toSql name]
  execWithClone conn sql args


renameFile ∷ DB → FileId → FileName → IO ()
renameFile conn fileId newName = do
  let sql = "UPDATE files " ++
            "SET    name = ? " ++
            "WHERE  id = ?"
      args = [toSql newName, toSql fileId]
  execWithClone conn sql args


mkFile ∷ DB → File → IO ()
mkFile conn (File name contents) = do
  let sql = "INSERT INTO files " ++
            "VALUES      (?, ?, ?)"
      args = [SqlNull, toSql name, toSql contents]
  execWithClone conn sql args


mkTag ∷ DB → Tag → IO ()
mkTag conn (Tag name) = do
  let sql = "INSERT INTO tags " ++
            "VALUES      (?, ?)"
      args = [SqlNull, toSql name]
  execWithClone conn sql args


rmTag ∷ DB → TagName → IO ()
rmTag conn name = do
  let sql = "DELETE FROM tags " ++
            "WHERE       name = ?"
      args = [toSql name]
  execWithClone conn sql args


-- ensureFileTag ∷ DB → FileId → TagId → IO ()
-- ensureFileTag conn fileId tagId =
--   catchSql res
--     (\e → do
--         dbg $ "ERROR: " ++ show e
--         if seNativeError e == 19
--           then return ()
--           else res)
--   where
--     res = mkFileTag conn fileId tagId


-- ^ find or make
ensureFileTag ∷ DB → FileId → TagId → IO ()
ensureFileTag db fileId tagId = do
  let sql = "SELECT  * " ++
            "FROM    files_tags " ++
            "WHERE   file_id = ? " ++
            "AND     tag_id  = ?"
  rows ← queryWithClone db sql [toSql fileId, toSql tagId]
  if null rows
    then mkFileTag db fileId tagId
    else return ()


mkFileTag ∷ DB → FileId → TagId → IO ()
mkFileTag conn fileId tagId = do
  let sql = "INSERT INTO files_tags " ++
            "VALUES      (?, ?, ?)"
      args = [SqlNull, toSql fileId, toSql tagId]
  execWithClone conn sql args


rmFileTag ∷ DB → FileId → TagName → IO ()
rmFileTag conn fileId tagName = do
  dbg $ "rmFileTag: " ++ tagName
  let sql = "DELETE FROM files_tags " ++
            "WHERE       file_id = ? " ++
            "AND         tag_id IN " ++
            "( SELECT tags.id " ++
            "  FROM   tags " ++
            "  WHERE  tags.name = ? )"
      args = [toSql fileId, toSql tagName]
  execWithClone conn sql args


-- rm File (and all associated FileTags)
rmFile ∷ DB → FileId → IO ()
rmFile conn fileId = do
  let sql1 = "DELETE FROM files " ++
             "WHERE       id = ? "
      sql2 = "DELETE FROM files_tags " ++
             "WHERE       file_id = ? "
  flip mapM_ [sql1, sql2]
    (\s → execWithClone conn s [toSql fileId])

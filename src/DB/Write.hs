{-# LANGUAGE UnicodeSyntax              #-}

module DB.Write
  ( mkFile
  , mkTag
  , mkFileTag
  , updateFile
  , rmFileTag
  , rmFile
  ) where


import           Database.HDBC

import           DB.Base
import           Debug


updateFile ∷ DB → File → IO ()
updateFile conn (File name contents) = do
  let sql = "UPDATE files " ++
            "SET    contents = ? " ++
            "WHERE  name = ?"
      args = [toSql contents, toSql name]
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

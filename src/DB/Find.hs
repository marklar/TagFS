{-# LANGUAGE UnicodeSyntax              #-}

module DB.Find where

-- import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B

import           Control.Monad            (liftM)
import           Data.Maybe               (isJust, catMaybes)
import           Data.List                (intercalate, intersect)
import           Database.HDBC            ( SqlValue, quickQuery'
                                          , fromSql, toSql
                                          , prepare, run, execute
                                          , catchSql
                                          )
import           Database.HDBC.Sqlite3    (Connection)

import           Debug
import           DB.Model


fileIdsForTag ∷ Connection → TagName → IO [FileId]
fileIdsForTag conn tagName = do
  dbg $ "!!! query: " ++ tagName

  tagIds ←
    catchSql (quickQuery' conn
               "SELECT tags.id FROM tags WHERE tags.name = ?"
               [toSql tagName]) (\e → do
                                    dbg $ "catch: " ++ show e
                                    return [])
    
  dbg $ "!!! after tagIds query: " ++ show tagIds
  
  ids ← quickQuery' conn
        ( "SELECT      files_tags.file_id " ++
          "FROM        files_tags " ++
          "INNER JOIN  tags " ++
          "ON          tags.name LIKE ? " ++
          "AND         files_tags.tag_id = tags.id" )
        [toSql tagName]
  dbg $ "!!! ids: " ++ show ids
  return $ map fromSql (concat ids)


filesFromTags ∷ Connection → [TagName] → IO [Entity]
filesFromTags conn tagNames = do
  dbg $ "!!! Find.filesFromTags: " ++ show tagNames
  fileIdLists ← mapM (fileIdsForTag conn) tagNames
  dbg $ "!!! after queries"
  
  let tagName2fileIds = zip tagNames fileIdLists
  dbg $ "tagName2fileIds: " ++ show tagName2fileIds

  -- find ∩ of fileIds lists
  let fileIds = foldr (\ids acc → intersect ids acc) [] fileIdLists

  maybeEntities ← mapM (fileEntityById conn) fileIds
  return $ catMaybes maybeEntities      


fileEntityById ∷ Connection → FileId → IO (Maybe Entity)
fileEntityById conn fileId = do
  maybeRow ← findRowById conn "files" fileId
  case maybeRow of
    Nothing →
      return Nothing
    Just [_, name, contents] →
      return $ Just (FileEntity fileId
                      (File (fromSql name) (fromSql contents)))



{- | Search DB for file w/ name and all its tag names.
-}
fileEntityFromTagsAndName ∷ Connection
                          → [TagName]    -- ^ Tag names
                          → FileName     -- ^ File name
                          → IO (Maybe Entity)
fileEntityFromTagsAndName conn tagNames name = do
  if name == "._."
    then return Nothing
    else do dbg $ "Find.fileEntityFromTagsAndName, tagNames: " ++ show tagNames
            allTagNames ← tagsForFileName conn name
            dbg $ "  allTagNames: " ++ show allTagNames
            if all (\n → elem n allTagNames) tagNames
              then fileEntityNamed conn name
              else return Nothing


{- | Helper fn. Sometimes when you have the name of a file, you want to
   know how it's tagged.
-}
tagsForFileName ∷ Connection → FileName → IO [TagName]
tagsForFileName conn fileName = do
  dbg $ "Find.tagsForFileName: " ++ show fileName
  maybeFileEntity ← fileEntityNamed conn fileName
  dbg $ "  maybeFileEntity: " ++ show maybeFileEntity
  case maybeFileEntity of
    Nothing →
      return []
    Just (FileEntity fileId _) → do
      dbg $ "  inner join"
      r ← quickQuery' conn ( "SELECT     ts.name " ++
                             "FROM       tags ts " ++
                             "INNER JOIN files_tags fts " ++
                             "ON         ts.id = fts.tag_id " ++
                             "AND        fts.file_id = ?"
                           ) [toSql fileId]
      return $ map (fromSql . head) r


-------------------------

-- Find FileEntity. If not ∃, "<fileName>: No such file or directory"
fileEntityNamed ∷ Connection → FileName → IO (Maybe Entity)
fileEntityNamed conn name = do
  dbg $ "Find.fileEntityNamed: " ++ name
  maybeRow ← findRowByName conn "files" name
  -- dbg $ "  maybeRow: " ++ show maybeRow
  case maybeRow of
    Nothing →
      return Nothing
    Just [id, _, contents] → do
      -- dbg $ "  id, contents: " ++ (fromSql id) ++ ", " ++ B.unpack (fromSql contents)
      return $ Just (FileEntity (fromSql id)
                      (File name (fromSql contents)))


tagEntityNamed ∷ Connection → TagName → IO (Maybe Entity)
tagEntityNamed conn name = do
  maybeRow ← findRowByName conn "tags" name
  case maybeRow of
    Nothing →
      return Nothing
    Just [id, _] →
      return . Just $ TagEntity (fromSql id) $ Tag name


-----------------------


fileExists ∷ Connection → FileName → IO Bool
fileExists = rowByNameExists "files"


tagExists ∷ Connection → TagName → IO Bool
tagExists = rowByNameExists "tags"


rowByNameExists ∷ String → Connection → String → IO Bool
rowByNameExists tableName conn nameVal =
  liftM isJust $ findRowByName conn tableName nameVal


----------------


findRowByName ∷ Connection
              → String   -- ^ table name
              → String   -- ^ name value
              → IO (Maybe [SqlValue])
findRowByName conn tableName name =
  findRowByVal conn tableName "name" (toSql name)


findRowById ∷ Connection
            → String   -- ^ table name
            → Integer  -- ^ row ID
            → IO (Maybe [SqlValue])
findRowById conn tableName id =
  findRowByVal conn tableName "id" (toSql id)
  

findRowByVal ∷ Connection
             → String   -- ^ table name
             → String   -- ^ column name
             → SqlValue
             → IO (Maybe [SqlValue])
findRowByVal conn tableName colName sqlVal = do
  dbg $ ">> Find.findRowByVal: " ++ fromSql sqlVal
  let sql = ( "SELECT * " ++
              "FROM   " ++ tableName ++ " " ++
              "WHERE  " ++ tableName ++ "." ++ colName ++ " = ? " ++
              "LIMIT  1" )
  dbg $ ">> sql: " ++ sql

  -- THIS IS WHERE IT BREAKS.
  
  r ← quickQuery' conn sql [sqlVal]
  dbg ">> after query"
  case r of
    [] → do
      return Nothing
    vals : _ → do
      return $ Just vals

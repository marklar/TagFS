{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UnicodeSyntax              #-}

module DbTinker where

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad (mapM_)
import           Data.ByteString
import           Data.Pool
-- import           Database.Persist (get, selectList, entityVal, insert, selectFirst, (==.), SelectOpt(..), Entity)
-- import           Database.Persist.Sqlite (runSqlite, runMigration, createSqlitePool)
-- import           Database.Persist.TH (share, mkPersist, sqlSettings, mkMigrate, persistLowerCase)
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import System.IO.Unsafe (unsafePerformIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Tag
  name String
    deriving Show
File
  name String
  contents ByteString
    deriving Show
FileTag
  fileId FileId
  tagId TagId
  UniqueFileTag fileId tagId
    deriving Show
|]

sqliteDB = ":memory:"

pool ∷ Pool SqlBackend
pool = unsafePerformIO $ createSqlitePool sqliteDB 10

runSQL sql = liftIO $  runSqlPersistMPool sql pool

-- migratesqlite = runSQL $ runMigration migrateAll


-- Everything inside a single call to `runSqlite`: single transaction.
dbTinker ∷ IO ()
dbTinker = do  -- runSql $ do
  -- runMigration migrateAll
  sportsTags ← runSQL $ selectList [TagName ==. "sports"] [LimitTo 5]
  liftIO $ print (sportsTags ∷ [Entity Tag])
  return ()


-- MonadIO - typeclass in which IO computations may be embedded.
-- method:   liftIO ∷ IO α → m α  -- Lift a computation from the IO monad.

createDb ∷ IO ()
createDb = runSqlite sqliteDB $ do
  runMigration migrateAll

  -- Files
  [footballId, futbolId] ←
    mapM insert [ File "football.txt" "Some amazing content about the Packers"
                , File "futbol.txt" "Me's contingut sorprendent sobre el Barc,a"
                ]

  -- Tags 
  [sportsId, barcaId, packersId] ←
    mapM insert [Tag "sports", Tag "barca", Tag "packers"]
  mapM_ (liftIO . print) [sportsId, barcaId, packersId]

  -- FileTags
  mapM_ insert [ FileTag footballId sportsId
               , FileTag footballId packersId
               , FileTag futbolId sportsId
               , FileTag futbolId barcaId
               ]

  -- selectFirst ∷ [Filter] → [SelectOpt] → m [(Key val, val)]
  -- FileTagTagId is a ctor. (Type ctor? Data ctor?)
  --   + uniquely identifies the `tag_id` column of the `file_tag` table
  --   + knows that `tag_id` is an Int (phantom)
  packersFileTag ←
    selectFirst [FileTagTagId ==. packersId] [Desc FileTagTagId]
  liftIO $ Prelude.putStrLn "'packers' FileTag entity"
  liftIO $ print packersFileTag

  
  -- sportsTags ← selectList [TagName ==. "sports"] [LimitTo 5]


  sportsFileTags ←
    selectList [FileTagTagId ==. sportsId] [LimitTo 5, Asc FileTagFileId]
  liftIO $ Prelude.putStrLn "'sports' FileTag entities"
  liftIO $ print sportsFileTags
  flip mapM_ sportsFileTags (\fileTagEntity → do  -- Entity: combo of db ID and val.
                                let fileId = (fileTagFileId . entityVal) fileTagEntity
                                file ← get fileId
                                liftIO $ print file)

  -- deleteWhere [FileTagTagId ==. packersId]
  -- delete packersId
  

type TagName = String
type Location = [TagName]  -- or Set Tag?


------------------------
------------------------
------------------------
-- Reading

{-
-- getFiles ∷ TagName → IO [File]
showFiles ∷ TagName → IO ()
showFiles tagName = runSqlite sqliteDB $ do
  -- runMigration --  migrateAll
  liftIO $ print ("tagName: " ++ tagName)
  sportsTags ← selectList [TagName ==. "sports"] [LimitTo 5]
  return ()
-}
{-  
  liftIO $ Prelude.putStrLn "'sports' FileTag entities"
  flip mapM_ sportsFileTags (\fileTagEntity → do  -- Entity: combo of db ID and val.
                                let fileId = (fileTagFileId . entityVal) fileTagEntity
                                file ← get fileId
                                liftIO $ print file)

  maybeTagEntity ← selectFirst [TagName ==. tagName] []
  case maybeTagEntity of
    Nothing → liftIO $ print "None" -- return []
    Just entity → liftIO $ print entity
--      let tagId = (tagFileId . entityVal) entity
--                  file ← get fileId
  -- selectList [FileTagTagId ==. tagId] [LimitTo 5, Asc FileTagFileId]
-}

{-

lsFiles ∷ [TagName] → IO [File]
lsFiles tagNames = runSqlite sqliteDB $ do
  flip mapM_ tagNames (\tagName → do
                          tagEntity ← selectFirst [TagTagId ==. packersId] [Desc FileTagTagId]

                          selectList
                          let tagId = (fileTagFileId . entityVal) fileTagEntity
  
  -- filters ← flip mapM tags (\tag → )
  -- tagIds ← flip mapM tags (\tag → )
  -- selectList [FileTagTagId ==. sportsId] [LimitTo 5, Asc FileTagFileId]

                          
lsDirs ∷ Location → IO [(Tag, Int)]
lsDirs = undefined


ls ∷ Location → IO ([File], [(Tag, Int)])
ls tags = do
  -- liftIO (,) (lsFiles tags) (lsDirs tags)
  files ← lsFiles tags
  dirs ← lsDirs tags
  return (files, dirs)


------------------------
-- Writing Files

{- | Removes from the specified file:
+ only the last tag in Location?
+ all of the tags in Location?
-}
rmFile ∷ Location → File → IO ()
rmFile = undefined

cpFile ∷ Location → Location → File → IO ()
cpFile srcLoc targLoc file = undefined

-- move ~ copy . delete
mvFile ∷ Location → Location → File → IO ()
mvFile srcLoc targLoc file = undefined


-------------------------
-- Writing Dirs

mkDir ∷ Location → ()
mkDir = undefined

-- for each file…
mvDir ∷ Location → ()
mvDir = undefined

-- for each file…
cpDir ∷ Location → ()
cpDir = undefined

-- for each file…
rmDir ∷ Location → ()
rmDir = undefined
-}

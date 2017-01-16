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
import           Data.ByteString
import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

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
    deriving Show
|]

-- MonadIO - typeclass in which IO computations may be embedded.
-- method:   liftIO ∷ IO α → m α  -- Lift a computation from the IO monad.
  
dbTinker ∷ IO ()
dbTinker = runSqlite ":memory:" $ do
  runMigration migrateAll

  -- Files
  footballId ← insert $ File "football.txt" "Some amazing content about the Packers"
  futbolId ← insert $ File "futbol.txt" "More amazing content about Barca"

  -- Tags 
  sportsId ← insert $ Tag "sports"
  sportsTag <- get sportsId
  liftIO $ print sportsTag

  barcaId ← insert $ Tag "barca"
  barcaTag <- get barcaId
  liftIO $ print barcaTag

  packersId ← insert $ Tag "packers"
  packersTag <- get packersId
  liftIO $ print packersTag

  -- FileTags
  insert $ FileTag footballId sportsId
  insert $ FileTag footballId packersId
  insert $ FileTag futbolId sportsId
  insert $ FileTag futbolId barcaId

  packersFileTag ← selectFirst [FileTagTagId ==. packersId] []
  liftIO $ Prelude.putStrLn "'packers' FileTag"
  liftIO $ print packersFileTag

  sportsFileTags ← selectList [FileTagTagId ==. sportsId] [LimitTo 5]
  liftIO $ Prelude.putStrLn "'sports' FileTag"
  -- liftIO $ print sportsTaggings
  flip mapM_ sportsFileTags (\fileTag {- Entity -} → do
                                let fileKey = fileTagFileId (entityVal fileTag)
                                file ← (get fileKey)
                                liftIO $ print file)

  -- delete janeId
  -- deleteWhere [BlogPostAuthorId ==. johnId]

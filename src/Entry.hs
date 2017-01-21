{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Entry where

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B
import           Data.Maybe
import           Control.Monad
import           Data.Monoid             ((<>))

import           System.Fuse
import           System.IO
import           System.Posix.Files
import           System.Posix.Types

import           Debug                   (dbg)
import           DataStore.Find
import           DataStore.Model
import           Parse
import           Stat                    ( dirStat, fileStat
                                         , tagGetFileSystemStats
                                         )
import           Types


findEntryByName ∷ Connection → String → IO (Maybe Entry)
findEntryByName dbConn name = do
  maybeFile ← findFileByName dbConn name
  if isJust maybeFile
    then return maybeFile
    else findTagByName dbConn name


findFileByName ∷ Connection → String → IO (Maybe Entry)
findFileByName dbConn name =
  fileFromName dbConn name >>= entityToEntry


findTagByName ∷ Connection → String → IO (Maybe Entry)
findTagByName dbConn name =
  tagFromName dbConn name >>= entityToEntry


entityToEntry ∷ Maybe Entity → IO (Maybe Entry)
entityToEntry maybeEntity = do
  ctx ← getFuseContext
  case maybeEntity of
    Just (FileEntity _ (File _ contents)) →
      return $ Just $ FileEntry (fileStat ctx) contents
    Just (TagEntity _ (Tag _)) →
      return $ Just $ DirEntry (dirStat ctx)
    Nothing →
      return Nothing

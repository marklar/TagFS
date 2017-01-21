{-# LANGUAGE UnicodeSyntax              #-}

module Tag where

import           Database.HDBC
import           Database.HDBC.Sqlite3
import           Find
import           Insert
import           Model


multiTagFile ∷ Connection → FileName → [TagName] → IO ()
multiTagFile conn fileName tagNames =
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
findOrCreateFileTag ∷ Connection
                    → Entity   -- ^ File
                    → Entity   -- ^ Tag
                    → IO ()
findOrCreateFileTag conn (FileEntity fileId _) (TagEntity tagId _) =
  mkFileTag conn fileId tagId
  

createFileTag ∷ Connection
              → Entity      -- ^ File
              → TagName
              → IO ()
createFileTag conn (FileEntity fileId _) tagName = do
  -- It *should* exist here.
  maybeTagEntity ← tagFromName conn tagName
  case maybeTagEntity of
    Nothing →
      -- FIXME: Should be error.
      return ()
    Just (TagEntity tagId (Tag name)) →
      mkFileTag conn fileId tagId

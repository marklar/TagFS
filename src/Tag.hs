{-# LANGUAGE UnicodeSyntax              #-}

module Tag where

import           DB.Find
import           DB.Insert
import           DB.Model


multiTagFile ∷ DB → FileName → [TagName] → IO ()
multiTagFile db fileName tagNames =
  mapM_ (tagFile db fileName) tagNames


tagFile ∷ DB → FileName → TagName → IO ()
tagFile db fileName tagName = do
  -- Find FileEntity. If ~∃, "<fileName>: No such file or directory"
  maybeFileEntity ← fileEntityNamed db fileName
  case maybeFileEntity of
    Nothing →
      -- FIXME: Return error.
      return ()
    Just fileEntity → do
      -- Find or create TagEntity
      maybeTagEntity ← tagEntityNamed db tagName
      case maybeTagEntity of
        Nothing → do
          mkTag db (Tag tagName)
          createFileTag db fileEntity tagName
        Just tagEntity →
          findOrCreateFileTag db fileEntity tagEntity


------------------

-- FIXME: What if FileTagEntity already exists?
findOrCreateFileTag ∷ DB
                    → Entity   -- ^ File
                    → Entity   -- ^ Tag
                    → IO ()
findOrCreateFileTag db (FileEntity fileId _) (TagEntity tagId _) =
  mkFileTag db fileId tagId
  

createFileTag ∷ DB
              → Entity      -- ^ File
              → TagName
              → IO ()
createFileTag db (FileEntity fileId _) tagName = do
  -- It *should* exist here.
  maybeTagEntity ← tagEntityNamed db tagName
  case maybeTagEntity of
    Nothing →
      -- FIXME: Should be error.
      return ()
    Just (TagEntity tagId (Tag name)) →
      mkFileTag db fileId tagId

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module File.Util
  ( tagFile
  , rmLastTag
  ) where


import           Data.Maybe              (catMaybes)

import           Debug                   (dbg)
import           DB.Base
import           DB.Read                 (tagEntityNamed)
import           DB.Write                (ensureFileTag, rmFileTag, rmFile)
import           Parse


-- Move to DB.Write?

tagFile ∷ DB → FileId → [TagName] → IO ()
tagFile db fileId tagNames = do
  maybeTagEntities ← mapM (tagEntityNamed db) tagNames
  let tagIds = map tagId (catMaybes maybeTagEntities)
  -- 'ensureFileTag' because might already be tagged w/ some of these tags.
  mapM_ (ensureFileTag db fileId) tagIds


-- THIS IS SPECIFICALLY FOR removeFile.
-- For last tagName of fromPath (if any), rm FileTag.
-- IF NO TAGS, THEN REMOVE FILE.
rmLastTag ∷ DB → FileId → FilePath → IO ()
rmLastTag db fileId filePath = do
  let (tagNames, _) = parseFilePath filePath
  if null tagNames
    then rmFile db fileId   -- & all associated FileTags
    else rmFileTag db fileId (last tagNames)

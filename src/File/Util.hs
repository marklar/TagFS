{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module File.Util
  ( tagFile
  ) where


import           Data.Maybe              (catMaybes)

import           Debug                   (dbg)
import           DB.Base
import           DB.Read                 (tagEntityNamed)
import           DB.Write                (mkFileTag)


-- Move to DB.Write?

tagFile ∷ DB → FileId → [TagName] → IO ()
tagFile db fileId tagNames = do
  maybeTagEntities ← mapM (tagEntityNamed db) tagNames
  let tagIds = map tagId (catMaybes maybeTagEntities)
  mapM_ (mkFileTag db fileId) tagIds



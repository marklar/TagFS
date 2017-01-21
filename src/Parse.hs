{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Parse where

import           Data.List.Split         (splitOn)

-- FIXME
import           DataStore.Model         (TagName, FileName)


{- | Sometimes the path is for a dir, other times for a file.
-}
parseFilePath ∷ FilePath → ([TagName], Maybe FileName)
parseFilePath path =
  case ps of
    [] →
      ([], Nothing)
    _ →
      (init ps, Just $ last ps)
  where ps = pathParts path


parseDirPath ∷ FilePath → [TagName]
parseDirPath = pathParts


pathParts ∷ FilePath → [String]
pathParts = filter (/= "") . splitOn "/"



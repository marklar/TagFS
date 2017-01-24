{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Parse where

import           Data.List.Split         (splitOn)
import           Data.List               (nub)

import           DB.Base                 (TagName, FileName)


{- | Sometimes the path is for a dir, other times for a file.
-}
parseFilePath ∷ FilePath → ([TagName], Maybe FileName)
parseFilePath path =
  case ps of
    [] →
      ([], Nothing)
    _ →
      (nub $ init ps, Just $ last ps)
  where ps = pathParts path


parseDirPath ∷ FilePath → [TagName]
parseDirPath = nub . pathParts


pathParts ∷ FilePath → [String]
pathParts = filter (/= "") . splitOn "/"



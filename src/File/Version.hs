{-# LANGUAGE UnicodeSyntax       #-}

module File.Version where

import           Text.Regex.PCRE
import qualified Data.ByteString.Char8   as B
import           System.Fuse
import           Data.List               (intercalate)
import           Data.List.Split         (splitOn)
import           Data.Maybe              (catMaybes)
import           System.Posix.Types      (FileMode, DeviceID)
import           System.FilePath.Posix   (splitExtension, addExtension)

import           Debug                   (dbg)
import           DB.Base
import           DB.Read                 (fileEntityNamed)
import           Types


{- | ∃ a file of this name already?
   e.g.:  foo.txt     ⇒  foo(1).txt
          foo(1).txt  ⇒  foo(2).txt
-}

-- Possibly versioned, that is. Usually won't need to change it.
versionedFileName ∷ DB → FileName → IO String
versionedFileName db fileName = do
  -- Does a file of this name already exist?
  --   If not, just return original.
  --   If so, does fileName end in "(\d)"?
  --     If not, simply tack on "(1)".
  --     If so, extract prev version, increment, and tack it on.
  maybeFileEntity ← fileEntityNamed db fileName
  case maybeFileEntity of
    Nothing →
      return fileName

    Just _ →
      versionedFileName db (newFileName fileName)
  

newFileName ∷ String → String
newFileName fileName =
  case getVersionNum base of
    Nothing →
      addExtension (base ++ "(1)") ext

    Just n →
      addExtension (incrVersion n base) ext
  where
    (base, ext) = splitExtension fileName
    replace old new = intercalate new . splitOn old
    incrVersion n = replace ("(" ++ show n ++ ")") ("(" ++ show (succ n) ++ ")")



getVersionNum ∷ String → Maybe Int
getVersionNum baseName =
  case getAllSubmatches match of
    _ : (off, len) : _ →
      Just (read (extract (off, len) baseName) ∷ Int)

    _ →
      Nothing
  where
    match =
      baseName =~ "\\((\\d*)\\)$" ∷ AllSubmatches [] (MatchOffset, MatchLength)



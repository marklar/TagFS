{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Types where

import           Data.ByteString         (ByteString)
import           System.Fuse             (FileStat)


data NonHandle = NonHandle

-- Each Entry, whether Dir or File, has a FileStat (from FUSE).
-- Files also have contents.
data Entry = FileEntry FileStat !ByteString
           | DirEntry  FileStat
  deriving Show


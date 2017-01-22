{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Types where

import           Data.ByteString         (ByteString)
import           System.Fuse             (FileStat)


data NonHandle = NonHandle

-- Each Entry, whether Dir or File, has a FileStat (from FUSE).
-- Files also have contents.
data Node = FileNode FileStat !ByteString
          | DirNode  FileStat
  deriving Show

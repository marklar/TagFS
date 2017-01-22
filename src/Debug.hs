{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Debug where

import           Data.ByteString         (ByteString)
import qualified Data.ByteString.Char8   as B
import           Data.List.Split         (splitOn)
import           Data.Monoid             ((<>))




-- Debugging. Instrument fns w/ log output to see what's happening.
-- IMPORTANT!!! Must be absolute path.
debugFile ∷ FilePath
debugFile = "/Users/markwong-vanharen/Development/TagFS/debug.log"

dbg ∷ String → IO ()
dbg msg = B.appendFile debugFile (B.pack msg <> "\n")

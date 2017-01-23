{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Debug where

import qualified Data.ByteString.Char8   as B
import           Data.Monoid             ((<>))


-- Debugging. Instrument fns w/ log output to see what's happening.
-- IMPORTANT!!! Must be absolute path.
debugFile ∷ FilePath
debugFile = "/tmp/tagFS.log"

dbg ∷ String → IO ()
dbg msg = B.appendFile debugFile (B.pack msg <> "\n")

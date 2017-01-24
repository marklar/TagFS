{-# LANGUAGE UnicodeSyntax              #-}


{- | Not currently in use.
-}
module Exists where

import           Control.Monad            (liftM)
import           Data.Maybe               (isJust)
import           DB.Base
import           DB.Row                   (findRowByName)


fileExists ∷ DB → FileName → IO Bool
fileExists = rowByNameExists "files"


tagExists ∷ DB → TagName → IO Bool
tagExists = rowByNameExists "tags"


rowByNameExists ∷ String → DB → String → IO Bool
rowByNameExists tableName conn nameVal =
  liftM isJust $ findRowByName conn tableName nameVal

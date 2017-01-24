{-# LANGUAGE UnicodeSyntax              #-}

module DB.Row
  ( findRowByName
  , findRowById
  ) where


import           Database.HDBC            (SqlValue, fromSql, toSql)
import           DB.Base


findRowByName ∷ DB
              → String   -- ^ table name
              → String   -- ^ name value
              → IO (Maybe [SqlValue])
findRowByName conn tableName name =
  findRowByVal conn tableName "name" (toSql name)


findRowById ∷ DB
            → String   -- ^ table name
            → Integer  -- ^ row ID
            → IO (Maybe [SqlValue])
findRowById conn tableName id =
  findRowByVal conn tableName "id" (toSql id)
  

----------------


findRowByVal ∷ DB
             → String   -- ^ table name
             → String   -- ^ column name
             → SqlValue
             → IO (Maybe [SqlValue])
findRowByVal conn tableName colName sqlVal = do
  let sql = "SELECT * " ++
            "FROM   " ++ tableName ++ " " ++
            "WHERE  " ++ tableName ++ "." ++ colName ++ " = ? " ++
            "LIMIT  1"
  r ← queryWithClone conn sql [sqlVal]
  case r of
    [] → do
      return Nothing
    vals : _ → do
      return $ Just vals

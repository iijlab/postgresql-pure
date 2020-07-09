module DataSource (connect) where

import           Data.Maybe               (fromMaybe)
import           Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)
import           System.Environment       (lookupEnv)

connect :: IO Connection
connect = do
  host <- getEnvDef "PURE_HOST" "127.0.0.1"
  port <- getEnvDef "PURE_PORT" "5432"
  user <- getEnvDef "PURE_USER" "postgres"
  password <- getEnvDef "PURE_PASSWORD" ""
  database <- getEnvDef "PURE_DATABASE" "postgres"
  connectPostgreSQL $ "host='" ++ host ++ "' port='" ++ port ++ "'user='" ++ user ++"' password = '" ++ password ++ "' dbname = '" ++ database ++ "'"

getEnvDef :: String -> String -> IO String
getEnvDef name value = fromMaybe value <$> lookupEnv name

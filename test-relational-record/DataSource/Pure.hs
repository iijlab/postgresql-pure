{-# LANGUAGE NamedFieldPuns #-}

module DataSource.Pure (connect) where

import           Data.Default.Class            (def)
import           Data.Maybe                    (fromMaybe)
import qualified Database.HDBC.PostgreSQL.Pure as Pure
import           System.Environment            (lookupEnv)

connect :: IO Pure.Connection
connect = do
  host <- getEnvDef "PURE_HOST" "127.0.0.1"
  port <- getEnvDef "PURE_PORT" "5432"
  user <- getEnvDef "PURE_USER" "postgres"
  password <- getEnvDef "PURE_PASSWORD" ""
  database <- getEnvDef "PURE_DATABASE" "postgres"
  let
    config =
      def
        { Pure.address = Pure.AddressNotResolved host port
        , Pure.user
        , Pure.password
        , Pure.database
        }
  Pure.connect config

getEnvDef :: String -> String -> IO String
getEnvDef name value = fromMaybe value <$> lookupEnv name

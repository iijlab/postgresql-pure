module SpecificDB where
import Database.HDBC
import Database.HDBC.PostgreSQL.Pure
import Test.HUnit
import Data.Default.Class(Default(def))
import Data.Maybe(fromMaybe)
import System.Environment(lookupEnv)

connectDB = 
    handleSqlError (do host <- getEnvDef "PURE_HOST" "127.0.0.1"
                       port <- getEnvDef "PURE_PORT" "5432"
                       user <- getEnvDef "PURE_USER" "postgres"
                       password <- getEnvDef "PURE_PASSWORD" ""
                       database <- getEnvDef "PURE_DATABASE" "postgres"
                       let
                         config =
                           def
                             { address = AddressNotResolved host port
                             , user = user
                             , password = password
                             , database = database
                             }
                       dbh <- connect config
                       run dbh "SET client_min_messages=WARNING" []
                       return dbh)

dateTimeTypeOfSqlValue :: SqlValue -> String
dateTimeTypeOfSqlValue (SqlLocalDate _) = "date"
dateTimeTypeOfSqlValue (SqlLocalTimeOfDay _) = "time without time zone"
dateTimeTypeOfSqlValue (SqlZonedLocalTimeOfDay _ _) = "time with time zone"
dateTimeTypeOfSqlValue (SqlLocalTime _) = "timestamp without time zone"
dateTimeTypeOfSqlValue (SqlZonedTime _) = "timestamp with time zone"
dateTimeTypeOfSqlValue (SqlUTCTime _) = "timestamp with time zone"
dateTimeTypeOfSqlValue (SqlDiffTime _) = "interval"
dateTimeTypeOfSqlValue (SqlPOSIXTime _) = "numeric"
dateTimeTypeOfSqlValue (SqlEpochTime _) = "integer"
dateTimeTypeOfSqlValue (SqlTimeDiff _) = "interval"
dateTimeTypeOfSqlValue _ = "text"

supportsFracTime = True

getEnvDef :: String -> String -> IO String
getEnvDef name value = fromMaybe value <$> lookupEnv name

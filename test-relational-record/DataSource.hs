module DataSource (connect) where

import           Data.Default.Class       (def)
import           Database.HDBC.PostgreSQL (Connection, connectPostgreSQL)

connect :: IO Connection
connect = connectPostgreSQL "user='postgres'"

module DataSource.Pure (connect) where

import           Data.Default.Class            (def)
import qualified Database.HDBC.PostgreSQL.Pure as Pure

connect :: IO Pure.Connection
connect = Pure.connect def

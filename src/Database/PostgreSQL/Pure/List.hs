{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- This is a list interface version of @Database.PostgreSQL.Pure@.
--
-- = Typical Example
--
-- Prepare a following table.
--
-- @
-- CREATE TABLE person (
--   id serial PRIMARY KEY,
--   name varchar(255) NOT NULL
-- );
-- INSERT INTO person (name) VALUES (\'Ada\');
-- @
--
-- You can run like following to get the record whose ID is 1.
--
-- >>> :set -XOverloadedStrings
-- >>> :set -XFlexibleContexts
-- >>> :set -XTypeApplications
-- >>>
-- >>> import Database.PostgreSQL.Pure.List
-- >>> import Data.Default.Class (def)
-- >>> import Data.Int (Int32)
-- >>> import Data.ByteString (ByteString)
-- >>> import Data.Tuple.Only (Only (Only))
-- >>> import Data.Maybe (fromMaybe)
-- >>> import System.Environment (lookupEnv)
-- >>>
-- >>> getEnvDef name value = fromMaybe value <$> lookupEnv name
-- >>>
-- >>> host' <- getEnvDef "PURE_HOST" "127.0.0.1"
-- >>> port' <- getEnvDef "PURE_PORT" "5432"
-- >>> user' <- getEnvDef "PURE_USER" "postgres"
-- >>> password' <- getEnvDef "PURE_PASSWORD" ""
-- >>> database' <- getEnvDef "PURE_DATABASE" "postgres"
-- >>>
-- >>> conn <- connect def { address = AddressNotResolved host' port', user = user', password = password', database = database' }
-- >>> preparedStatementProcedure = parse "" "SELECT id, name FROM person WHERE id = $1" (Left (1, 2))
-- >>> portalProcedure <- bind "" BinaryFormat BinaryFormat (parameters conn) (const $ fail "") (Only (1 :: Int32)) preparedStatementProcedure
-- >>> executedProcedure = execute @_ @(Int32, ByteString) 0 (const $ fail "") portalProcedure
-- >>> ((_, _, e, _), _) <- sync conn executedProcedure
-- >>> records e
-- [(1,"Ada")]
module Database.PostgreSQL.Pure.List
  ( -- * Connection
    Config (..)
  , Connection
  , pid
  , parameters
  , config
  , Address (..)
  , BackendParameters
  , Pid
  , withConnection
  , connect
  , disconnect
    -- * Extended Query
  , parse
  , bind
  , execute
  , flush
  , sync
  , close
  , PreparedStatement (name, parameterOids, resultInfos)
  , PreparedStatementProcedure (name, parameterOids)
  , PreparedStatementName (..)
  , Portal (name)
  , PortalProcedure (name)
  , PortalName (..)
  , Executed (result, records)
  , ExecutedProcedure
  , ExecuteResult (..)
  , CommandTag (..)
  , Query (..)
  , FormatCode (..)
  , ColumnInfo
  , Message
  , MessageResult
  , Bind
  , Execute
  , Close
  , StringEncoder
  , StringDecoder
    -- * Transaction
  , begin
  , commit
  , rollback
  , TransactionState (..)
    -- * Record
  , FromField (..)
  , FromRecord (..)
  , ToField (..)
  , ToRecord (..)
  , Raw (..)
    -- * Exception
  , Exception.Exception (..)
  , Exception.ErrorResponse (..)
  , Exception.ResponseParsingFailed (..)
    -- * OID
  , Oid
  ) where

import           Database.PostgreSQL.Pure.Internal.Connection (connect, disconnect, withConnection)
import           Database.PostgreSQL.Pure.Internal.Data       (Address (AddressNotResolved, AddressResolved),
                                                               BackendParameters, ColumnInfo,
                                                               CommandTag (BeginTag, CommitTag, CopyTag, CreateTableTag, DeleteTag, DropTableTag, FetchTag, InsertTag, MoveTag, RollbackTag, SelectTag, UpdateTag),
                                                               Config (Config, address, database, password, receptionBufferSize, sendingBufferSize, user),
                                                               Connection (config, parameters, pid),
                                                               ExecuteResult (ExecuteComplete, ExecuteEmptyQuery, ExecuteSuspended),
                                                               Executed, ExecutedProcedure,
                                                               FormatCode (BinaryFormat, TextFormat),
                                                               FromField (fromField), FromRecord (fromRecord),
                                                               MessageResult, Oid, Pid, Portal, PortalName (PortalName),
                                                               PortalProcedure, PreparedStatement,
                                                               PreparedStatementName (PreparedStatementName),
                                                               PreparedStatementProcedure, Query (Query),
                                                               Raw (Null, Value), StringDecoder, StringEncoder,
                                                               ToField (toField), ToRecord (toRecord),
                                                               TransactionState (Block, Failed, Idle))
import qualified Database.PostgreSQL.Pure.Internal.Data       as Data
import qualified Database.PostgreSQL.Pure.Internal.Exception  as Exception
import           Database.PostgreSQL.Pure.Internal.Query      (Bind (bind), Close (close), Execute (execute), Message,
                                                               begin, commit, flush, parse, rollback, sync)

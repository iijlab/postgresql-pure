{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeSynonymInstances  #-}

{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- This is a compatible interface with @HDBC-postgresql@'s @Database.HDBC.PostgreSQL@ except 'Config'.
--
-- Prepared statements are closed when some requests come once 'Statement's are GCed, because HDBC doesn't have “close” interface.
module Database.HDBC.PostgreSQL.Pure
  ( -- * Connection
    Config (..)
  , Connection (config)
  , Pure.Address (..)
  , withConnection
  , connect
    -- * Transaction
  , begin
  ) where

import           Database.HDBC.PostgreSQL.Pure.Parser         (convertQuestionMarkStyleToDollarSignStyle, splitQueries)

import qualified Database.PostgreSQL.Pure.Internal.Connection as Pure
import qualified Database.PostgreSQL.Pure.Internal.Data       as Pure
import qualified Database.PostgreSQL.Pure.Internal.Exception  as Pure
import qualified Database.PostgreSQL.Pure.Internal.MonadFail  as MonadFail
import qualified Database.PostgreSQL.Pure.List                as Pure
import qualified Database.PostgreSQL.Pure.Oid                 as Oid

import           Paths_postgresql_pure                        (version)

import           Database.HDBC                                (IConnection (clone, commit, dbServerVer, dbTransactionSupport, describeTable, disconnect, getTables, hdbcClientVer, hdbcDriverName, prepare, proxiedClientName, proxiedClientVer, rollback, run, runRaw),
                                                               SqlColDesc (SqlColDesc, colDecDigits, colNullable, colOctetLength, colSize, colType),
                                                               SqlError (SqlError, seErrorMsg, seNativeError, seState),
                                                               SqlTypeId, throwSqlError)
import           Database.HDBC.ColTypes                       (SqlInterval (SqlIntervalSecondT), SqlTypeId (SqlBigIntT, SqlBitT, SqlCharT, SqlDateT, SqlDecimalT, SqlDoubleT, SqlFloatT, SqlIntervalT, SqlTimeT, SqlTimeWithZoneT, SqlTimestampT, SqlTimestampWithZoneT, SqlUnknownT, SqlVarBinaryT, SqlVarCharT))
import           Database.HDBC.Statement                      (SqlValue (SqlBool, SqlByteString, SqlChar, SqlDiffTime, SqlDouble, SqlInt32, SqlInt64, SqlInteger, SqlLocalDate, SqlLocalTime, SqlLocalTimeOfDay, SqlNull, SqlPOSIXTime, SqlRational, SqlString, SqlUTCTime, SqlWord32, SqlWord64, SqlZonedLocalTimeOfDay, SqlZonedTime),
                                                               Statement (Statement, describeResult, execute, executeMany, executeRaw, fetchRow, finish, getColumnNames, originalQuery))

import           Control.Concurrent                           (MVar, modifyMVar_, newMVar)
import           Control.Exception.Safe                       (Exception (displayException, fromException, toException),
                                                               impureThrow, try)
import           Control.Monad                                (unless, void)
import qualified Data.ByteString                              as BS
import qualified Data.ByteString.Short                        as BSS
import qualified Data.ByteString.UTF8                         as BSU
import           Data.Convertible                             (Convertible (safeConvert), convert)
import           Data.Default.Class                           (Default (def))
import           Data.Foldable                                (for_)
import           Data.Int                                     (Int32, Int64)
import           Data.IORef                                   (IORef, mkWeakIORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict                              as M
import           Data.Maybe                                   (fromMaybe)
import           Data.Scientific                              (FPFormat (Exponent), Scientific, formatScientific,
                                                               fromRationalRepetend)
import           Data.String                                  (IsString (fromString))
import           Data.Time                                    (DiffTime, NominalDiffTime, zonedTimeToUTC)
import           Data.Traversable                             (for)
import           Data.Tuple.Only                              (Only (Only))
import           Data.Typeable                                (Typeable, cast)
import           Data.Version                                 (showVersion)
import           Data.Word                                    (Word32, Word64)
import qualified PostgreSQL.Binary.Encoding                   as BE

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail                           (MonadFail)
#endif

-- | A configuration of a connection.
--
-- Default configuration is 'def', which is following.
--
-- >>> address def
-- AddressResolved 127.0.0.1:5432
-- >>> user def
-- "postgres"
-- >>> password def
-- ""
-- >>> database def
-- ""
-- >>> sendingBufferSize def
-- 4096
-- >>> receptionBufferSize def
-- 4096
--
-- @
-- encodeString def = \code -> case code of \"UTF8\" -> 'pure' . 'BSU.fromString'; _ -> 'const' $ 'fail' $ "unexpected character code: " <> 'show' code
-- decodeString def = \code -> case code of \"UTF8\" -> 'pure' . 'BSU.toString'; _ -> 'const' $ 'fail' $ "unexpected character code: " <> 'show' code
-- @
data Config =
  Config
    { address             :: Pure.Address
    , user                :: String
    , password            :: String
    , database            :: String
    , sendingBufferSize   :: Int -- ^ in byte
    , receptionBufferSize :: Int -- ^ in byte
    , encodeString        :: BSS.ShortByteString -> Pure.StringEncoder
    , decodeString        :: BSS.ShortByteString -> Pure.StringDecoder
    }

instance Show Config where
  show Config { address, user, password, database, sendingBufferSize, receptionBufferSize } =
    mconcat
      [ "Config { address = "
      , show address
      , ", user = "
      , show user
      , ", password = "
      , show password
      , ", database = "
      , show database
      , ", sendingBufferSize = "
      , show sendingBufferSize
      , ", receptionBufferSize = "
      , show receptionBufferSize
      , ", encodeString = <function>, decodeString = <function> }"
      ]

instance Default Config where
  def =
    let Pure.Config { address, user, password, database, sendingBufferSize, receptionBufferSize } = def
    in
      Config
        { address
        , user
        , password
        , database
        , sendingBufferSize
        , receptionBufferSize
        , encodeString = \code -> case code of "UTF8" -> pure . BSU.fromString; _ -> const $ fail $ "unexpected character code: " <> show code
        , decodeString = \code -> case code of "UTF8" -> pure . BSU.toString; _ -> const $ fail $ "unexpected character code: " <> show code
        }

-- | PostgreSQL connection.
data Connection =
  Connection
    { connection                    :: Pure.Connection
    , statementCounter              :: IORef Word
    , unnecessaryPreparedStatemtnts :: MVar [Pure.PreparedStatement] -- To accumulate unnecessary prepared statements
                                                                     -- to dispose them when some requests come,
                                                                     -- because HDBC doesn't have a interface to close statements.
    , config                        :: Config
    }

-- | Bracket function for a connection.
withConnection :: Config -> (Connection -> IO a) -> IO a
withConnection config@Config { address, user, password, database, sendingBufferSize, receptionBufferSize } f =
  Pure.withConnection Pure.Config { address, user, password, database, sendingBufferSize, receptionBufferSize } $ \c -> do
    void $ Pure.sync c Pure.begin
    conn <- Connection c <$> newIORef 0 <*> newMVar [] <*> pure config
    f conn

-- | To connect to the server.
connect :: Config -> IO Connection
connect config@Config { address, user, password, database, sendingBufferSize, receptionBufferSize } = do
  c <- Pure.connect Pure.Config { address, user, password, database, sendingBufferSize, receptionBufferSize }
  void $ Pure.sync c Pure.begin
  Connection c <$> newIORef 0 <*> newMVar [] <*> pure config

instance IConnection Connection where
  disconnect = Pure.disconnect . connection

  commit hc@Connection { connection } =
    convertException $ do
      closeUnnecessaryPreparedStatemtnts hc
      void $ Pure.sync connection (Pure.commit, Pure.begin)

  rollback hc@Connection { connection } =
    convertException $ do
      closeUnnecessaryPreparedStatemtnts hc
      void $ Pure.sync connection (Pure.rollback, Pure.begin)

  run hc@Connection { connection = connection@Pure.Connection { parameters }, config = Config { encodeString, decodeString } } query values =
    convertException $ do
      closeUnnecessaryPreparedStatemtnts hc
      charCode <- lookupClientEncoding parameters
      let
        encode = encodeString charCode
        decode = decodeString charCode
        queryQS :: Pure.Query
        queryQS = fromString query
        queryDS =
          case convertQuestionMarkStyleToDollarSignStyle queryQS of
            Right q -> q
            Left err -> impureThrow $ RequestBuildingFailed $ "conversion from question mark style to dollar sign style: " <> err
      ps <- Pure.flush connection $ Pure.parse "" queryDS (Left (0, 0)) -- footnote [1]
      let
        pProc = forceBind $ Pure.bind "" Pure.TextFormat Pure.TextFormat parameters encode values ps
        eProc = Pure.execute @_ @() 0 decode pProc
      (_, _, e, _) <- Pure.flush connection eProc
      pure $ resultCount $ Pure.result e

  runRaw hc@Connection { connection = connection@Pure.Connection { parameters }, config = Config { encodeString, decodeString } } query =
    convertException $ do
      closeUnnecessaryPreparedStatemtnts hc
      charCode <- lookupClientEncoding parameters
      let
        encode = encodeString charCode
        decode = decodeString charCode
        queries = splitQueries $ fromString query
      for_ queries $ \q -> do
        ps <- Pure.flush connection $ Pure.parse "" q (Left (0, 0)) -- footnote [1]
        Pure.flush connection $ Pure.execute @_ @() 0 decode $ forceBind $ Pure.bind "" Pure.TextFormat Pure.TextFormat parameters encode ([] :: [SqlValue]) ps

  prepare hc@Connection { connection = connection@Pure.Connection { parameters }, statementCounter, unnecessaryPreparedStatemtnts, config = Config { encodeString, decodeString } } query =
    convertException $ do
      closeUnnecessaryPreparedStatemtnts hc
      count <- incrementCounter statementCounter
      portalCounter <- newIORef 0
      charCode <- lookupClientEncoding parameters
      let
        encode = encodeString charCode
        decode = decodeString charCode
        encodeIO = MonadFail.fromEither . encode :: String -> IO BS.ByteString
        decodeIO = MonadFail.fromEither . decode :: BS.ByteString -> IO String
      queryBS <- encodeIO query
      let
        queryDS =
          case convertQuestionMarkStyleToDollarSignStyle $ Pure.Query queryBS of
            Right q -> q
            Left err -> impureThrow $ RequestBuildingFailed $ "conversion from question mark style to dollar sign style: " <> err
      countBS <- encodeIO $ show count
      let
        psName = Pure.PreparedStatementName $ countBS <> ": " <> queryBS
      (preparedStatement, _) <- Pure.sync connection $ Pure.parse psName queryDS (Left (0, 0)) -- footnote [1]
      portalsRef <- newIORef [] :: IO (IORef [(Maybe [SqlValue], Pure.Portal)])
      alive <- newIORef () -- see the document of 'keepPreparedStatementAlive'
      let
        execute :: [SqlValue] -> IO Integer
        execute values =
          convertException $ do
            closeUnnecessaryPreparedStatemtnts hc
            finish'
            count <- incrementCounter portalCounter
            countBS <- encodeIO $ show count
            let
              pName =
                case psName of
                  Pure.PreparedStatementName n -> Pure.PortalName $ countBS <> ": " <> n
            ((_, p, e, _), _) <- Pure.sync connection $ Pure.execute 1 decode $ forceBind $ Pure.bind pName Pure.TextFormat Pure.TextFormat parameters encode values preparedStatement
            keepPreparedStatementAlive
            writeIORef portalsRef $
              case Pure.result e of
                Pure.ExecuteSuspended -> [(Just $ head $ Pure.records e, p)]
                _                     -> []
            pure $ resultCount $ Pure.result e

        executeRaw :: IO ()
        executeRaw = void $ execute []

        executeMany :: [[SqlValue]] -> IO ()
        executeMany valuess =
          convertException $ do
            closeUnnecessaryPreparedStatemtnts hc
            finish'
            eProcs <-
              for valuess $ \values -> do
                count <- incrementCounter portalCounter
                countBS <- encodeIO $ show count
                let
                  pName =
                    case psName of
                      Pure.PreparedStatementName n -> Pure.PortalName $ countBS <> ": " <> n
                pure $ Pure.execute 1 decode $ forceBind $ Pure.bind pName Pure.TextFormat Pure.TextFormat parameters encode values preparedStatement
            (rs, _) <- Pure.sync connection eProcs
            keepPreparedStatementAlive
            writeIORef portalsRef $
              mconcat $
                (<$> rs) $ \(_, p, e, _) ->
                  case Pure.result e of
                    Pure.ExecuteSuspended -> [(Just $ head $ Pure.records e, p)]
                    _                     -> mempty

        finish :: IO ()
        finish =
          convertException $ do
            closeUnnecessaryPreparedStatemtnts hc
            finish'
            keepPreparedStatementAlive
            writeIORef portalsRef []

        finish' :: IO ()
        finish' = do
          ps <- readIORef portalsRef
          unless (null ps) $ void $ Pure.sync connection $ Pure.close . snd <$> ps

        fetchRow :: IO (Maybe [SqlValue])
        fetchRow =
          convertException $ do
            closeUnnecessaryPreparedStatemtnts hc
            ps <- readIORef portalsRef
            case ps of
              (Just r, p):ps -> do
                writeIORef portalsRef ((Nothing, p):ps)
                pure $ Just r
              (Nothing, p):ps -> do
                ((_, _, e, _), _) <- Pure.sync connection $ Pure.execute 1 decode p
                keepPreparedStatementAlive
                case Pure.result e of
                  Pure.ExecuteSuspended ->
                    pure $ Just $ head $ Pure.records e
                  _ -> do
                    void $ Pure.sync connection $ Pure.close p
                    keepPreparedStatementAlive
                    writeIORef portalsRef ps
                    pure Nothing
              [] -> pure Nothing

        getColumnNames :: IO [String]
        getColumnNames =
          convertException $ do
            closeUnnecessaryPreparedStatemtnts hc
            sequence $ decodeIO . #name <$> Pure.resultInfos preparedStatement

        originalQuery :: String
        originalQuery = query

        describeResult :: IO [(String, SqlColDesc)]
        describeResult =
          convertException $ do
            closeUnnecessaryPreparedStatemtnts hc
            let
              columnInfos = Pure.resultInfos preparedStatement
              psProc = Pure.parse "attr" "SELECT attnotnull FROM pg_attribute WHERE attrelid = $1 AND attnum = $2" (Right ([Oid.oid, Oid.int2], [Oid.bool]))
            (ps, _) <- Pure.sync connection psProc
            results <-
              for columnInfos $ \Pure.ColumnInfo { name, tableOid, attributeNumber, typeOid, typeLength, typeModifier } -> do
                ((_, _, e, _), _) <- Pure.sync connection $ Pure.execute 1 decode $ forceBind $ Pure.bind "" Pure.TextFormat Pure.TextFormat parameters encode (tableOid, attributeNumber) ps
                let
                  (Only attnotnull) = head $ Pure.records e
                  (colSize, colDecDigits) = columnSize typeOid typeLength typeModifier
                nameStr <- decodeIO name
                pure
                  ( nameStr
                  , SqlColDesc
                      { colType = convert typeOid
                      , colSize
                      , colOctetLength = Nothing
                      , colDecDigits
                      , colNullable = Just $ not attnotnull
                      }
                  )
            void $ Pure.sync connection $ Pure.close ps
            keepPreparedStatementAlive
            pure results

        -- The GHC optimiser make 'Statement's be GCed in advance of exiting those scopes.
        -- To prevent it, insert 'readIORef's at the end of actions.
        -- Finalisers of 'IORef's are set, instead of ones of 'Statement's.
        -- See: https://github.com/snoyberg/http-client/pull/352
        keepPreparedStatementAlive :: IO ()
        keepPreparedStatementAlive = void $ readIORef alive

        statement =
          Statement
            { execute
            , executeRaw
            , executeMany
            , finish
            , fetchRow
            , getColumnNames
            , originalQuery
            , describeResult
            }

      -- set up a finaliser
      void $ mkWeakIORef alive $ modifyMVar_ unnecessaryPreparedStatemtnts $ pure . (preparedStatement:)
      pure statement

  clone hc@Connection { config } =
    convertException $ do
      closeUnnecessaryPreparedStatemtnts hc
      connect config

  hdbcDriverName _ = "postgresql"

  hdbcClientVer _ = showVersion version

  proxiedClientName = hdbcDriverName

  proxiedClientVer = hdbcClientVer

  dbServerVer Connection { connection = Pure.Connection { parameters }, config = Config { decodeString } } =
    fromMaybe "" $ do
      serverVersion <- M.lookup "server_version" parameters
      decode <- decodeString <$> lookupClientEncoding parameters
      MonadFail.fromEither $ decode $ BSS.fromShort serverVersion

  dbTransactionSupport _ = True

  getTables hc@Connection { connection = connection@Pure.Connection { parameters }, config = Config { encodeString, decodeString } } =
    convertException $ do
      closeUnnecessaryPreparedStatemtnts hc
      charCode <- lookupClientEncoding parameters
      let
        encode = encodeString charCode
        decode = decodeString charCode
        decodeIO = MonadFail.fromEither . decode :: BS.ByteString -> IO String
        q :: Pure.Query
        q = "SELECT table_name FROM information_schema.tables WHERE table_schema != 'pg_catalog' AND table_schema != 'information_schema'"
      ((_, _, e, _), _) <- Pure.sync connection $ Pure.execute 0 decode $ forceBind $ Pure.bind "" Pure.TextFormat Pure.TextFormat parameters encode () $ Pure.parse "" q (Right ([], [Oid.sqlIdentifier]))
      sequence $ decodeIO . (\(Only (Pure.SqlIdentifier str)) -> str) <$> Pure.records e

  describeTable hc@Connection { connection = connection@Pure.Connection { parameters }, config = Config { encodeString, decodeString } } tableName =
    convertException $ do
      closeUnnecessaryPreparedStatemtnts hc
      charCode <- lookupClientEncoding parameters
      let
        encode = encodeString charCode
        decode = decodeString charCode
        decodeIO = MonadFail.fromEither . decode :: BS.ByteString -> IO String
        q :: Pure.Query
        q = "SELECT attname, atttypid, attlen, atttypmod, attnotnull FROM pg_attribute, pg_class, pg_namespace WHERE attnum > 0 AND attisdropped IS FALSE AND attrelid = pg_class.oid AND relnamespace = pg_namespace.oid AND relname = $1 ORDER BY attnum"
      ((_, _, e, _), _) <-
        Pure.sync connection $ Pure.execute 0 decode $ forceBind $ Pure.bind "" Pure.TextFormat Pure.TextFormat parameters encode (Only tableName) $ Pure.parse "" q (Right ([Oid.name], [Oid.name, Oid.oid, Oid.int2, Oid.int4, Oid.bool]))
      for (Pure.records e) $ \(attname, atttypid, attlen, atttypmod, attnotnull) -> do
        let
          typeLength = case attlen of { (-1) -> Pure.VariableLength ; _ -> Pure.FixedLength attlen }
          (colSize, colDecDigits) = columnSize atttypid typeLength atttypmod
        attnameBS <- decodeIO attname
        pure
          ( attnameBS
          , SqlColDesc
              { colType = convert atttypid
              , colSize
              , colOctetLength = Nothing
              , colDecDigits
              , colNullable = Just $ not attnotnull
              }
          )

-- | To send @BEGIN@ SQL statement.
begin :: Connection -> IO ()
begin hc@Connection { connection } =
  convertException $ do
    closeUnnecessaryPreparedStatemtnts hc
    void $ Pure.sync connection Pure.begin

columnSize :: Pure.Oid -> Pure.TypeLength -> Pure.TypeModifier -> (Maybe Int, Maybe Int)
columnSize typeOid Pure.VariableLength typeModifier
  | typeOid `elem` [Oid.bpchar, Oid.varchar] = (Just $ fromIntegral typeModifier - 4, Nothing) -- minus header size
  | typeOid == Oid.numeric = let (p, q) = (fromIntegral typeModifier - 4) `divMod` (2 ^ (16 :: Int) :: Int) in (Just p, Just q)
  | otherwise = (Nothing, Nothing)
columnSize _ (Pure.FixedLength l) _ = (Just $ fromIntegral l, Nothing)

forceBind :: Either String Pure.PortalProcedure -> Pure.PortalProcedure
forceBind (Right a)  = a
forceBind (Left err) = impureThrow $ RequestBuildingFailed err

incrementCounter :: IORef Word -> IO Word
incrementCounter ref = do
  n <- readIORef ref
  writeIORef ref (n + 1)
  pure n

closeUnnecessaryPreparedStatemtnts :: Connection -> IO ()
closeUnnecessaryPreparedStatemtnts Connection { connection, unnecessaryPreparedStatemtnts } =
  modifyMVar_ unnecessaryPreparedStatemtnts $ \pss -> do
    unless (null pss) $ void $ Pure.sync connection $ Pure.close <$> pss
    pure []

convertException :: IO a -> IO a
convertException a = do
  r <- try a
  case r of
    Right v -> pure v
    Left e -> throwSqlError $ SqlError { seState = "", seNativeError = -1, seErrorMsg = displayException (e :: Pure.Exception) }

newtype RequestBuildingFailed = RequestBuildingFailed { message :: String } deriving (Show, Read, Eq, Typeable)

instance Exception RequestBuildingFailed where
  toException = toException . Pure.Exception
  fromException = ((\(Pure.Exception e) -> cast e) =<<) . fromException

instance Pure.FromField SqlValue where
  fromField _ _ Nothing = pure SqlNull
  fromField decode info@Pure.ColumnInfo { typeOid } v
    | typeOid == Oid.char
    = SqlChar <$> Pure.fromField decode info v
    | typeOid `elem` [Oid.bpchar, Oid.varchar, Oid.text, Oid.name]
    = SqlByteString <$> Pure.fromField decode info v
    | typeOid `elem` [Oid.int2, Oid.int4]
    = SqlInt32 <$> Pure.fromField decode info v
    | typeOid == Oid.int8
    = SqlInt64 <$> Pure.fromField decode info v
    | typeOid == Oid.bool
    = SqlBool <$> Pure.fromField decode info v
    | typeOid `elem` [Oid.float4, Oid.float8]
    = SqlDouble <$> Pure.fromField decode info v
    | typeOid == Oid.numeric
    = SqlRational . toRational @Scientific <$> Pure.fromField decode info v
    | typeOid == Oid.date
    = SqlLocalDate <$> Pure.fromField decode info v
    | typeOid == Oid.time
    = SqlLocalTimeOfDay <$> Pure.fromField decode info v
    | typeOid == Oid.timetz
    = uncurry SqlZonedLocalTimeOfDay <$> Pure.fromField decode info v
    | typeOid == Oid.timestamp
    = SqlLocalTime <$> Pure.fromField decode info v
    | typeOid == Oid.timestamptz
    = SqlUTCTime <$> Pure.fromField decode info v
    | typeOid == Oid.interval
    = SqlDiffTime . fromRational . toRational @DiffTime <$> Pure.fromField decode info v
    | typeOid == Oid.oid
    = SqlInt32 . (\(Oid.Oid n) -> n) <$> Pure.fromField decode info v
    | otherwise = fail $ "unsupported type: " <> show typeOid

instance Pure.ToField SqlValue where
  toField backendParams encode oid format (SqlString v) = Pure.toField backendParams encode oid format v
  toField backendParams encode oid format (SqlByteString v) = Pure.toField backendParams encode oid format v
  toField backendParams encode oid format (SqlWord32 v) = Pure.toField backendParams encode oid format $ fromIntegral @Word32 @Int32 v -- may get overflow
  toField backendParams encode oid format (SqlWord64 v) = Pure.toField backendParams encode oid format $ fromIntegral @Word64 @Int64 v -- may get overflow
  toField backendParams encode oid format (SqlInt32 v) = Pure.toField backendParams encode oid format v
  toField backendParams encode oid format (SqlInt64 v)
    | fromIntegral (minBound :: Int32) <= v && v <= fromIntegral (maxBound :: Int32) = Pure.toField backendParams encode oid format (fromIntegral v :: Int32)
    | otherwise = Pure.toField backendParams encode oid format v
  toField backendParams encode oid format (SqlInteger v) = Pure.toField backendParams encode oid format $ fromInteger @Scientific v
  toField backendParams encode oid format (SqlChar v) = Pure.toField backendParams encode oid format [v]
  toField backendParams encode oid format (SqlBool v) = Pure.toField backendParams encode oid format v
  toField backendParams encode oid format (SqlDouble v) = Pure.toField backendParams encode oid format v
  toField backendParams encode oid format (SqlRational v) = Pure.toField backendParams encode oid format v
  toField backendParams encode oid format (SqlLocalDate v) = Pure.toField backendParams encode oid format v
  toField backendParams encode oid format (SqlLocalTimeOfDay v) = Pure.toField backendParams encode oid format v
  toField backendParams encode oid format (SqlZonedLocalTimeOfDay t tz) = Pure.toField backendParams encode oid format (t, tz)
  toField backendParams encode oid format (SqlLocalTime v) = Pure.toField backendParams encode oid format v
  toField backendParams encode oid format (SqlZonedTime v) = Pure.toField backendParams encode oid format $ zonedTimeToUTC v
  toField backendParams encode oid format (SqlUTCTime v) = Pure.toField backendParams encode oid format v
  toField backendParams encode oid format (SqlDiffTime v) = Pure.toField backendParams encode oid format $ fromRational @DiffTime $ toRational @NominalDiffTime v
  toField backendParams encode oid format (SqlPOSIXTime v) = Pure.toField backendParams encode oid format v
  toField _ _ _ Pure.TextFormat SqlNull = pure Nothing
  toField _ _ _ _ _ = fail "unsupported" -- SqlEpochTime and SqlTimeDiff are deprecated

-- | Security risk of DoS attack.
--
-- You should convert 'Rational' to 'Scientific' with 'fromRationalRepetend' in the user side.
-- If the rational value is computed to repeating decimals like 1/3 = 0.3333…, this consumes a lot of memories.
-- This is provided because of the HDBC compatibility.
instance Pure.ToField Rational where
  toField _ encode Nothing format v =
    let
      s =
        case fromRationalRepetend Nothing v of
          Left (s, _)  -> s
          Right (s, _) -> s
    in
      case format of
        Pure.TextFormat   -> Just <$> MonadFail.fromEither (encode $ formatScientific Exponent Nothing s)
        Pure.BinaryFormat -> pure $ Just $ BE.encodingBytes $ BE.numeric s
  toField backendParams encode (Just o) f v | o == Oid.numeric = Pure.toField backendParams encode Nothing f v
                                            | otherwise = fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: Rational"

resultCount :: Pure.ExecuteResult -> Integer
resultCount e =
  toInteger $
    case e of
      Pure.ExecuteComplete tag ->
        case tag of
          Pure.InsertTag _ n  -> n
          Pure.DeleteTag n    -> n
          Pure.UpdateTag n    -> n
          Pure.SelectTag _    -> 0
          Pure.MoveTag n      -> n
          Pure.FetchTag n     -> n
          Pure.CopyTag n      -> n
          Pure.CreateTableTag -> 0
          Pure.DropTableTag   -> 0
          Pure.BeginTag       -> 0
          Pure.CommitTag      -> 0
          Pure.RollbackTag    -> 0
          Pure.SetTag         -> 0
      Pure.ExecuteEmptyQuery -> 0
      Pure.ExecuteSuspended -> 0

instance Convertible Pure.Oid SqlTypeId where
  safeConvert oid | oid `elem` [Oid.int2, Oid.int4, Oid.int8] = pure SqlBigIntT
                  | oid == Oid.numeric = pure SqlDecimalT
                  | oid == Oid.float4 = pure SqlFloatT
                  | oid == Oid.float8 = pure SqlDoubleT
                  | oid `elem` [Oid.char, Oid.bpchar] = pure SqlCharT
                  | oid `elem` [Oid.varchar, Oid.text] = pure SqlVarCharT
                  | oid == Oid.bytea = pure SqlVarBinaryT
                  | oid == Oid.timestamp = pure SqlTimestampT
                  | oid == Oid.timestamptz = pure SqlTimestampWithZoneT
                  | oid == Oid.date = pure SqlDateT
                  | oid == Oid.time = pure SqlTimeT
                  | oid == Oid.timetz = pure SqlTimeWithZoneT
                  | oid == Oid.interval = pure $ SqlIntervalT SqlIntervalSecondT
                  | oid == Oid.bool = pure SqlBitT
                  | otherwise = pure $ SqlUnknownT $ show oid

lookupClientEncoding :: MonadFail m => Pure.BackendParameters -> m BSS.ShortByteString
lookupClientEncoding params =
  case M.lookup "client_encoding" params of
    Nothing   -> fail "\"client_encoding\" backend parameter not found"
    Just code -> pure code

-- Footnote
-- [1] Dirty hack: The numbers 0 and 0 are not used, when the prepared statement procedure is not given to "bind".

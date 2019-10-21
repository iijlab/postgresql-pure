{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}

{-# OPTIONS_GHC -Wno-orphans                 #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-} -- for postgresql-typed's pgSQL quasiquotes

import qualified Database.PostgreSQL.Simple                   as S
#if !MIN_VERSION_postgresql_simple(0,6,0)
import qualified Database.PostgreSQL.Simple.FromField         as S
import qualified Database.PostgreSQL.Simple.FromRow           as S
#endif

import qualified Database.PostgreSQL.Pure                     as P
import qualified Database.PostgreSQL.Pure.Oid                 as P

import qualified Database.PostgreSQL.Typed                    as T

#ifndef mingw32_HOST_OS
import qualified Database.PostgreSQL.Driver                   as W
import qualified Database.PostgreSQL.Protocol.Codecs.Decoders as WD
import qualified Database.PostgreSQL.Protocol.DataRows        as W
import qualified Database.PostgreSQL.Protocol.Decoders        as WD
import qualified Database.PostgreSQL.Protocol.Store.Decode    as WD
#endif

import qualified RepeatThreadPool                             as Pool

import           Control.Applicative                          ((<|>))
import           Control.Concurrent                           (threadDelay)
import           Control.DeepSeq                              (NFData (rnf), deepseq)
import           Control.Exception                            (AsyncException (ThreadKilled), catchJust, throwIO)
import           Control.Monad                                (void)
import qualified Data.Attoparsec.ByteString                   as AP
import qualified Data.Attoparsec.ByteString.Char8             as APC
import           Data.ByteString                              (ByteString)
import qualified Data.ByteString.Char8                        as BSC
import qualified Data.ByteString.Lazy                         as BSL
import qualified Data.ByteString.UTF8                         as BSU
import qualified Data.Csv                                     as Csv
import           Data.Default.Class                           (def)
import           Data.Functor                                 (($>))
import           Data.Hourglass                               (TimeFormatElem (Format_Day2, Format_Hour, Format_Minute, Format_Month2, Format_Second, Format_Text, Format_TzHM, Format_Year),
                                                               TimeFormatString (TimeFormatString), timePrint)
import           Data.Int                                     (Int32, Int64)
import           Data.IORef
import           Data.List                                    (sortOn)
import           Data.Maybe                                   (fromJust, fromMaybe)
import           Data.Proxy                                   (Proxy (Proxy))
import           Data.Scientific
import           Data.String                                  (IsString)
import           Data.Time
import           Data.Traversable                             (for)
import           GHC.Generics                                 (Generic)
import           System.CPUTime                               (getCPUTime)
import           System.Environment                           (lookupEnv)
import           System.Hourglass                             (timeCurrent)
import           System.IO                                    (IOMode (WriteMode), withFile)
import           System.Random.Shuffle                        (shuffleM)
import           Time.System                                  (timeCurrentP)
import           Time.Types                                   (Elapsed (Elapsed), ElapsedP (ElapsedP),
                                                               NanoSeconds (NanoSeconds), Seconds (Seconds))

#if !MIN_VERSION_postgresql_typed(0,6,0)
import           Network                                      (PortID (PortNumber))
#endif

main :: IO ()
main = do
  current <- timeCurrent
  host <- getEnvDef "PB_HOST" "localhost"
  concurrencies <- parseEnvDef  "PB_CONCURRENCY" [10] (listParser concurrencyParser)
  period <- parseEnvDef "PB_PERIOD" 60 periodParser
  sample <- readEnvDef "PB_SAMPLE" 1
  libraries <- parseEnvDef "PB_LIBRARY" measures (listParser libraryParser)
  patterns <- shuffleM $ (,,) <$> libraries <*> concurrencies <*> [0 .. sample - 1]
  dat <-
    for patterns $ \((name, measure), concurrency, nth) -> do
      let config = Config { concurrency, period, host }
      putStrLn $ "target: " <> name <> ", concurrency: " <> show concurrency <> ", nth: " <> show nth
      Result cpuTime tps <- measure config
      pure $ ResultRecord name concurrency nth cpuTime tps
  let
    sortedDat =
      sortOn
        (\ResultRecord { methodName, concurrency, nth } -> (methodName, concurrency, nth))
        dat
  let csv = Csv.encodeDefaultOrderedByName sortedDat
  withFile ("constant-" <> timePrint timeFormat current <> ".csv") WriteMode $ flip BSL.hPutStr csv

listParser :: AP.Parser a -> AP.Parser [a]
listParser p = ((:) <$> (p <* APC.char ',' <* AP.many' APC.space) <*> listParser p) <|> ((:[]) <$> p)

concurrencyParser :: AP.Parser Word
concurrencyParser = APC.decimal

periodParser :: AP.Parser Word
periodParser = do
  n <- APC.decimal
  unit <- (APC.char 's' $> 1) <|> (APC.char 'm' $> 60)
  pure $ n * unit

libraryParser :: AP.Parser (String, Config -> IO Result)
libraryParser =
  ("pure" $> ("pure", measurePure))
  <|> ("simple" $> ("simple", measureSimple))
  <|> ("typed" $> ("typed", measureTyped))
  <|> ("wire" $> ("wire", measureWire))

query :: IsString a => a
query = "SELECT 2147483647 :: int4, 9223372036854775807 :: int8, 1234567890.0123456789 :: numeric, 0.015625 :: float4, 0.00024414062 :: float8, 'hello' :: varchar, 'hello' :: text, '\\xDEADBEEF' :: bytea, '1000-01-01 00:00:00.000001' :: timestamp, '2000-01-01 00:00:00.000001+14:30' :: timestamptz, '0001-01-01' :: date, '23:00:00' :: time, true :: bool"

measures :: [(String, Config -> IO Result)]
measures =
  [ ("pure", measurePure)
  , ("simple", measureSimple)
  , ("typed", measureTyped)
#ifndef mingw32_HOST_OS
  , ("wire", measureWire)
#endif
  ]

data PureConnection = PureConnection { psRef :: IORef (Maybe (P.PreparedStatement 0 13)), connection :: P.Connection }

measurePure :: Config -> IO Result
measurePure config@Config { host } = do
  let
    pureConfig =
      def
        { P.user = "postgres"
        , P.database = "tiny_tpcc"
        , P.address = P.AddressNotResolved host "5432"
        }
  doMeasure
    config
    (PureConnection <$> newIORef Nothing <*> P.connect pureConfig)
    (P.disconnect . connection)
    $ \PureConnection { psRef, connection } -> do
        mps <- readIORef psRef
        case mps of
          Nothing -> do
            let
              resultOids = (P.int4, P.int8, P.numeric, P.float4, P.float8, P.varchar, P.text, P.bytea, P.timestamp, P.timestamptz, P.date, P.time, P.bool)
              psProc = P.parse "ps" (P.Query query) (Just (Proxy, resultOids)) :: P.PreparedStatementProcedure 0 13
              pProc = fromJust $ P.bind "" P.BinaryFormat P.BinaryFormat (P.parameters connection) (const $ fail "") () psProc :: P.PortalProcedure 0 13
              eProc = P.execute 0 (const $ fail "") pProc :: P.ExecutedProcedure 0 13 (Int32, Int64, Scientific, Float, Double, ByteString, ByteString, ByteString, LocalTime, UTCTime, Day, TimeOfDay, Bool)
            ((ps, _, e, _), _) <- P.sync connection eProc
            deepseq (P.records e) $ pure ()
            writeIORef psRef $ Just ps
          Just ps -> do
            let
              pProc = fromJust $ P.bind "" P.BinaryFormat P.BinaryFormat (P.parameters connection) (const $ fail "") () ps :: P.PortalProcedure 0 13
              eProc = P.execute 0 (const $ fail "") pProc :: P.ExecutedProcedure 0 13 (Int32, Int64, Scientific, Float, Double, ByteString, ByteString, ByteString, LocalTime, UTCTime, Day, TimeOfDay, Bool)
            void $ P.sync connection eProc

measureSimple :: Config -> IO Result
measureSimple config@Config { host } = do
  let libpqParam = "user='postgres' dbname='tiny_tpcc' host='" <> BSU.fromString host <> "'"
  doMeasure
    config
    (S.connectPostgreSQL libpqParam)
    S.close
    $ \conn -> do
        r <- S.query_ conn query :: IO [(Int32, Int64, Scientific, Float, Double, ByteString, ByteString, ByteString, LocalTime, UTCTime, Day, TimeOfDay, Bool)]
        deepseq r $ pure ()

measureTyped :: Config -> IO Result
measureTyped config@Config { host } = do
  let
    postgresqlConfig =
      T.defaultPGDatabase
        { T.pgDBUser = "postgres"
        , T.pgDBName = "tiny_tpcc"
#if MIN_VERSION_postgresql_typed(0,6,0)
        , T.pgDBAddr = Left (host, "5432")
#else
        , T.pgDBHost = host
        , T.pgDBPort = PortNumber 5432
#endif
        }
  doMeasure
    config
    (T.pgConnect postgresqlConfig)
    T.pgDisconnect
    $ \conn -> do
        r <-
          T.pgQuery
            conn
            [T.pgSQL|! SELECT 2147483647 :: int4, 9223372036854775807 :: int8, 1234567890.0123456789 :: numeric, 0.015625 :: float4, 0.00024414062 :: float8, 'hello' :: varchar, 'hello' :: text, '\xDEADBEEF' :: bytea, '1000-01-01 00:00:00.000001' :: timestamp, '2000-01-01 00:00:00.000001+14:30' :: timestamptz, '0001-01-01' :: date, '23:00:00' :: time, true :: bool |]
            :: IO [(Int32, Int64, Scientific, Float, Double, ByteString, ByteString, ByteString, LocalTime, UTCTime, Day, TimeOfDay, Bool)]
        deepseq r $ pure ()

measureWire :: Config -> IO Result
#ifdef mingw32_HOST_OS
measureWire = error "postgres-wire can run on only UNIX-like environments"
#else
measureWire config@Config { host } = do
  let
    wireConfig =
      W.defaultConnectionSettings
        { W.settingsUser = "postgres"
        , W.settingsDatabase = "tiny_tpcc"
        , W.settingsHost = BSU.fromString host
        , W.settingsPort = 5432
        }
  doMeasure
    config
    (either (error . show) id <$> W.connect wireConfig)
    W.close
    $ \conn -> do
        W.sendBatchAndSync conn [W.Query query [] W.Binary W.Binary W.AlwaysCache]
        rows <- either (error . show) id <$> W.readNextData conn
        let
          decoder = do
            void WD.decodeHeader
            void WD.getInt16BE
            (,,,,,,,,,,,,)
              <$> WD.getNonNullable WD.int4
              <*> WD.getNonNullable WD.int8
              <*> WD.getNonNullable WD.numeric
              <*> WD.getNonNullable WD.float4
              <*> WD.getNonNullable WD.float8
              <*> WD.getNonNullable WD.bsText
              <*> WD.getNonNullable WD.bsText
              <*> WD.getNonNullable WD.bytea
              <*> WD.getNonNullable WD.timestamp
              <*> WD.getNonNullable WD.timestamptz
              <*> WD.getNonNullable WD.date
              <*> WD.getNonNullable WD.time
                <*> WD.getNonNullable WD.bool
          records = W.decodeManyRows decoder rows
        deepseq records $ pure ()
        either (error . show) id <$> W.waitReadyForQuery conn
#endif

doMeasure :: Config -> IO conn -> (conn -> IO ()) -> (conn -> IO ())-> IO Result
doMeasure Config { concurrency, period } makeConn disposeConn target = do
  time <- timeCurrentP -- nanoseconds
  cpuTime <- getCPUTime -- picoseconds
  pool <-
    Pool.start
      concurrency
      ( do
          conn <- makeConn
          pure (conn, 0 :: Word)
      )
      ( \(conn, count) ->
          catchJust
            (\e -> if e == ThreadKilled then Just e else Nothing)
            (target conn $> (conn, count + 1))
            (\e -> disposeConn conn *> throwIO e)
      )
  threadDelay $ fromIntegral period * 1000 * 1000 -- microseconds
  time' <- timeCurrentP
  cpuTime' <- getCPUTime
  (_, counts) <- unzip <$> Pool.kill pool
  let
    count = sum counts
    actualPeriod@(ElapsedP (Elapsed (Seconds apSecs)) (NanoSeconds apNanosecs)) = time' - time
    cpuPeriod = cpuTime' - cpuTime
    (cpuSecs, cpuPicosecs) = cpuPeriod `divMod` (1000 ^ (4 :: Int))
    tps' = fromIntegral count / (fromIntegral apSecs + fromIntegral apNanosecs / 1000 ^ (3 :: Int)) :: Double
  putStrLn $ "measurement period: " <> show actualPeriod
  putStrLn $ "CPU time: " <> show cpuSecs <> "s " <> show cpuPicosecs <> "ps"
  putStrLn $ "transactions per second: " <> show tps'
  pure $ Result cpuPeriod tps'

data Config =
  Config
    { concurrency :: Word -- ^ the number of threads
    , period      :: Word -- ^ measurement period in second
    , host        :: String
    } deriving (Show, Read, Eq)

data ResultRecord =
  ResultRecord
    { methodName  :: String
    , concurrency :: Word
    , nth         :: Word
    , cpuTime     :: Integer
    , tps         :: Double
    }
  deriving (Show, Read, Generic)

data Result = Result { cpuTime :: Integer, tps :: Double } deriving (Show, Read, Eq)

instance Csv.FromNamedRecord ResultRecord
instance Csv.ToNamedRecord ResultRecord
instance Csv.DefaultOrdered ResultRecord

getEnvDef :: String -> String -> IO String
getEnvDef name value = fromMaybe value <$> lookupEnv name

readEnvDef :: Read a => String -> a -> IO a
readEnvDef name value = maybe value read <$> lookupEnv name

parseEnvDef :: String -> a -> AP.Parser a -> IO a
parseEnvDef name value parser = do
  mstr <- lookupEnv name
  case mstr of
    Nothing -> pure value
    Just str ->
      case AP.parseOnly parser $ BSC.pack str of
        Left e  -> error $ "parseEnv " <> name <> ": " <> e
        Right a -> pure a

timeFormat :: TimeFormatString
timeFormat =
  TimeFormatString
    [ Format_Year, dash, Format_Month2, dash, Format_Day2 -- date
    , Format_Text 'T'
    , Format_Hour, dash, Format_Minute, dash, Format_Second -- time
    , Format_TzHM
    ]
  where
    dash = Format_Text '-'

instance
  (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7, NFData a8, NFData a9, NFData a10, NFData a11, NFData a12, NFData a13)
  => NFData ((,,,,,,,,,,,,) a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13) where
  rnf (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13) = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5 `seq` rnf x6 `seq` rnf x7 `seq` rnf x8 `seq` rnf x9 `seq` rnf x10 `seq` rnf x11 `seq` rnf x12 `seq` rnf x13

#if !MIN_VERSION_postgresql_simple(0,6,0)
-- 13-tuple
instance
  ( S.FromField a, S.FromField b, S.FromField c, S.FromField d, S.FromField e, S.FromField f, S.FromField g
  , S.FromField h, S.FromField i, S.FromField j, S.FromField k, S.FromField l, S.FromField m)
  => S.FromRow (a, b, c, d, e, f, g, h, i, j, k, l, m) where
  fromRow = (,,,,,,,,,,,,) <$> S.field <*> S.field <*> S.field <*> S.field <*> S.field <*> S.field <*> S.field
                           <*> S.field <*> S.field <*> S.field <*> S.field <*> S.field <*> S.field
#endif

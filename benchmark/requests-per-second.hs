{- original: https://github.com/postgres-haskell/postgres-wire/blob/master/bench/Bench.hs -}

{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -Wno-orphans#-}

import           Control.Concurrent
import           Control.DeepSeq                              (NFData, deepseq, rnf)
import           Control.Monad
import           Data.ByteString                              (ByteString)
import qualified Data.ByteString                              as B
import qualified Data.ByteString.Char8                        as BC
import           Data.Default.Class                           (def)
import           Data.Foldable
import           Data.Int
import           Data.IORef
import           Data.Maybe
import           Data.Proxy
import           Data.Scientific                              (Scientific)
import           Data.Time                                    (Day, DiffTime, LocalTime, TimeOfDay, TimeZone, UTCTime)
import           Data.Tuple.Homotuple.Only                    ()
import           Data.Tuple.Only
import           Options.Applicative
import           System.Clock
import           System.Environment                           (lookupEnv)

import qualified Database.PostgreSQL.LibPQ                    as LibPQ

import qualified Database.PostgreSQL.Pure                     as Pure
import qualified Database.PostgreSQL.Pure.Oid                 as Pure

#ifndef mingw32_HOST_OS
#if !MIN_VERSION_base(4,13,0)
import qualified Data.ByteString.Lazy                         as BL

import           Database.PostgreSQL.Driver
import qualified Database.PostgreSQL.Protocol.Codecs.Decoders as WD
import           Database.PostgreSQL.Protocol.DataRows
import qualified Database.PostgreSQL.Protocol.Decoders        as WD
import qualified Database.PostgreSQL.Protocol.Store.Decode    as WD
import           Database.PostgreSQL.Protocol.Types
#endif
#endif

{-
CREATE TABLE _bytes_100_of_1k(b bytea);
CREATE TABLE _bytes_400_of_200(b bytea);
CREATE TABLE _bytes_10_of_20k(b bytea);
CREATE TABLE _bytes_1_of_200(b bytea);
CREATE TABLE _bytes_300_of_100(b bytea);

INSERT INTO _bytes_100_of_1k(b)
  (SELECT repeat('a', 1000)::bytea FROM generate_series(1, 100));

INSERT INTO _bytes_400_of_200(b)
  (SELECT repeat('a', 200)::bytea FROM generate_series(1, 400));

INSERT INTO _bytes_10_of_20k(b)
  (SELECT repeat('a', 20000)::bytea FROM generate_series(1, 10));

INSERT INTO _bytes_1_of_200(b) VALUES(repeat('a', 200)::bytea);

INSERT INTO _bytes_300_of_100(b)
 (SELECT repeat('a', 100)::bytea FROM generate_series(1, 300));
-}

data Action
    = BenchPW RowsType
    | BenchPure RowsType
    | BenchLibPQ RowsType
    | BenchLoop
    deriving (Show, Eq)

data RowsType
    = Bytes100_1k
    | Bytes400_200
    | Bytes10_20k
    | Bytes1_200
    | Bytes300_100
    | Constant
    deriving (Show, Eq)

data Config =
  Config
    { host     :: String
    , database :: String
    , user     :: String
    , password :: String
    }
  deriving (Show, Read, Eq)

cli :: Parser Action
cli = hsubparser $
       cmd "pw" "benchmark postgres-wire" (BenchPW <$> rowTypeParser)
    <> cmd "pure" "benchmark postgresql-pure" (BenchPure <$> rowTypeParser)
    <> cmd "libpq" "benchmark libpq" (BenchLibPQ <$> rowTypeParser)
    <> cmd "loop" "benchmark datarows decoding loop" (pure BenchLoop)
  where
    cmd c h p = command c (info (helper <*> p) $ header h)
    rowTypeParser = hsubparser $
           cmd "b100_1k"  "100 rows of 1k bytes"  (pure Bytes100_1k)
        <> cmd "b400_200" "400 rows of 200 bytes" (pure Bytes400_200)
        <> cmd "b10_20k"  "10 rows of 20k bytes"  (pure Bytes10_20k)
        <> cmd "b1_200"   "1 row of 200 bytes"    (pure Bytes1_200)
        <> cmd "b300_100" "300 rows of 100 bytes" (pure Bytes300_100)
        <> cmd "constant" "constant values"       (pure Constant)

main :: IO ()
main = do
  host <- getEnvDef "PB_HOST" "localhost"
  database <- getEnvDef "PB_DB" "postgres"
  user <- getEnvDef "PB_USER" "postgres"
  password <- getEnvDef "PB_PASSWORD" ""
  act <- execParser (info (helper <*> cli) $ header "Postgres-wire benchmark")
  execAction Config { host, database, user, password } act

execAction :: Config -> Action -> IO ()
execAction config (BenchPW rows)    = benchPw config rows
execAction config (BenchPure rows)  = benchPure config rows
execAction config (BenchLibPQ rows) = benchLibpq config rows
execAction config BenchLoop         = benchLoop config

queryStatement :: RowsType -> B.ByteString
queryStatement = \case
    Bytes100_1k  -> "SELECT * from _bytes_100_of_1k"
    Bytes400_200 -> "SELECT * from _bytes_400_of_200"
    Bytes10_20k  -> "SELECT * from _bytes_10_of_20k"
    Bytes1_200   -> "SELECT * from _bytes_1_of_200"
    Bytes300_100 -> "SELECT * from _bytes_300_of_100"
    Constant     -> "SELECT 2147483647 :: int4, 9223372036854775807 :: int8, 1234567890.0123456789 :: numeric, 0.015625 :: float4, 0.00024414062 :: float8, 'hello' :: varchar, 'hello' :: text, '\\xDEADBEEF' :: bytea, '1000-01-01 00:00:00.000001' :: timestamp, '2000-01-01 00:00:00.000001+14:30' :: timestamptz, '0001-01-01 BC' :: date, '24:00:00' :: time, '00:00:00+1459' :: timetz, '177999 millenniums 0.999999 sec' :: interval, true :: bool;"

benchPw :: Config -> RowsType -> IO ()
#ifdef mingw32_HOST_OS
benchPw = error "postgres-wire can run on only UNIX-like environments"
#elif MIN_VERSION_base(4,13,0)
benchPw = error "postgres-wire is not compatible with base >= 4.13.0"
#else
benchPw Config { host, database, user, password } rowsType =
  benchRequests createConnection $ \c ->
    case rowsType of
      Constant -> do
        sendBatchAndSync c [q]
        rows <- either (error . show) id <$> readNextData c
        let
          decoder = do
            void WD.decodeHeader
            void WD.getInt16BE
            (,,,,,,,,,,,,,,)
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
              <*> WD.getNonNullable WD.timetz
              <*> WD.getNonNullable WD.interval
              <*> WD.getNonNullable WD.bool
          records = decodeManyRows decoder rows
        deepseq records $ pure ()
        waitReadyForQuery c
      _ -> do
        sendBatchAndSync c [q]
        void $ readNextData c
        waitReadyForQuery c
  where
    statement = queryStatement rowsType
    q = Query statement [] Binary Binary AlwaysCache
    createConnection = connect defaultSettings >>=
        either (error . ("Connection error " <>) . show) pure

    defaultSettings = defaultConnectionSettings
        { settingsHost     = BC.pack host
        , settingsDatabase = BC.pack database
        , settingsUser     = BC.pack user
        , settingsPassword = BC.pack password
        }
#endif

benchPure :: Config -> RowsType -> IO ()
benchPure Config { host, database, user, password } rowsType =
  case rowsType of
    Constant ->
      benchRequests ((,) <$> connect <*> newIORef Nothing) $ \(c, psRef) -> do
        mps <- readIORef psRef
        case mps of
          Nothing -> do
            let
              resultOids = (Pure.int4, Pure.int8, Pure.numeric, Pure.float4, Pure.float8, Pure.varchar, Pure.text, Pure.bytea, Pure.timestamp, Pure.timestamptz, Pure.date, Pure.time, Pure.timetz, Pure.interval, Pure.bool)
              psProc = Pure.parse "ps" (Pure.Query statement) (Just (Proxy, resultOids)) :: Pure.PreparedStatementProcedure 0 15
              pProc = fromJust $ Pure.bind "" Pure.BinaryFormat Pure.BinaryFormat (Pure.parameters c) (const $ fail "") () psProc :: Pure.PortalProcedure 0 15
              eProc = Pure.execute 0 (const $ fail "") pProc :: Pure.ExecutedProcedure 0 15 (Int32, Int64, Scientific, Float, Double, ByteString, ByteString, ByteString, LocalTime, UTCTime, Day, TimeOfDay, (TimeOfDay, TimeZone), DiffTime, Bool)
            ((ps, _, e, _), _) <- Pure.sync c eProc
            deepseq (Pure.records e) $ pure ()
            writeIORef psRef $ Just ps
          Just ps -> do
            let
              pProc = fromJust $ Pure.bind "" Pure.BinaryFormat Pure.BinaryFormat (Pure.parameters c) (const $ fail "") () ps :: Pure.PortalProcedure 0 15
              eProc = Pure.execute 0 (const $ fail "") pProc :: Pure.ExecutedProcedure 0 15 (Int32, Int64, Scientific, Float, Double, ByteString, ByteString, ByteString, LocalTime, UTCTime, Day, TimeOfDay, (TimeOfDay, TimeZone), DiffTime, Bool)
            void $ Pure.sync c eProc
    _ ->
      benchRequests ((,) <$> connect <*> newIORef Nothing) $ \(c, psRef) -> do
        mps <- readIORef psRef
        case mps of
          Nothing -> do
            let
              psProc = Pure.parse "ps" (Pure.Query statement) (Just (Proxy, Only Pure.bytea)) :: Pure.PreparedStatementProcedure 0 1
              pProc = fromJust $ Pure.bind "" Pure.BinaryFormat Pure.BinaryFormat (Pure.parameters c) (const $ fail "") () psProc :: Pure.PortalProcedure 0 1
              eProc = Pure.execute 0 (const $ fail "") pProc :: Pure.ExecutedProcedure 0 1 (Only Pure.Raw)
            ((ps, _, _, _), _) <- Pure.sync c eProc
            writeIORef psRef $ Just ps
          Just ps -> do
            let
              pProc = fromJust $ Pure.bind "" Pure.BinaryFormat Pure.BinaryFormat (Pure.parameters c) (const $ fail "") () ps :: Pure.PortalProcedure 0 1
              eProc = Pure.execute 0 (const $ fail "") pProc :: Pure.ExecutedProcedure 0 1 (Only Pure.Raw)
            void $ Pure.sync c eProc
  where
    statement = queryStatement rowsType
    connect =
      Pure.connect
        def
          { Pure.user = user
          , Pure.password = password
          , Pure.database = database
          , Pure.address = Pure.AddressNotResolved host "5432"
          }

benchLibpq :: Config -> RowsType -> IO ()
benchLibpq Config { host, database, user, password } rowsType = benchRequests libpqConnection $ \c -> do
    r <- fromJust <$> LibPQ.execPrepared c "" [] LibPQ.Binary
    rows <- LibPQ.ntuples r
    parseRows r (rows - 1)
  where
    statement = queryStatement rowsType
    libpqConnection = do
        conn <- LibPQ.connectdb $ "host='" <> BC.pack host <> "' user='" <> BC.pack user <> "' dbname='" <> BC.pack database <> "' password='" <> BC.pack password <> "'"
        Just result <- LibPQ.prepare conn "" statement Nothing
        status <- LibPQ.resultStatus result
        unless (status == LibPQ.CommandOk) $ error "prepare failed"
        pure conn
    parseRows _ (-1) = pure ()
    parseRows r n    = LibPQ.getvalue r n 0 >> parseRows r (n - 1)

benchRequests :: IO c -> (c -> IO a) -> IO ()
benchRequests connectAction queryAction = do
    results <- replicateM 8 newThread
    threadDelay $ durationSeconds * 1000 * 1000
    for_ results $ \(_, _, tid) -> killThread tid
    s <- sum <$> traverse (\(ref, _, _) -> readIORef ref) results
    latency_total <- sum <$> traverse (\(_, ref, _) -> readIORef ref) results

    putStrLn $ "Requests per second: " ++ show (s `div` durationSeconds)
    putStrLn $ "Average latency [ms]: " ++ displayLatency latency_total s
  where
    durationSeconds :: Int
    durationSeconds = 10
    newThread  = do
        ref_count   <- newIORef 0 :: IO (IORef Int)
        ref_latency <- newIORef 0 :: IO (IORef Int64)
        c <- connectAction
        tid <- forkIO $ forever $ do
            t1 <- getTime Monotonic
            r <- queryAction c
            r `seq` pure ()
            t2 <- getTime Monotonic
            modifyIORef' ref_latency (+ getDifference t2 t1)
            modifyIORef' ref_count (+1)
        pure (ref_count, ref_latency, tid)

    getDifference (TimeSpec end_s end_ns) (TimeSpec start_s start_ns) =
        (end_s - start_s) * 1000000000 + end_ns - start_ns

    displayLatency latency reqs =
        let a = latency `div` fromIntegral reqs
            (ms, ns) = a `divMod` 1000000
        in show ms <> "." <> show ns

benchLoop :: Config -> IO ()
#ifdef mingw32_HOST_OS
benchLoop = error "postgres-wire can run on only UNIX-like environments"
#elif MIN_VERSION_base(4,13,0)
benchLoop = error "postgres-wire is not compatible with base >= 4.13.0"
#else
benchLoop _config = do
    counter <- newIORef 0  :: IO (IORef Word)
    content <- newIORef "" :: IO (IORef BL.ByteString)
    -- File contains a PostgreSQL binary response on the query:
    --   "SELECT typname, typnamespace, typowner, typlen, typbyval,
    --          typcategory, typispreferred, typisdefined, typdelim,
    --          typrelid, typelem, typarray from pg_type"
    !bs <- B.readFile "bench/pg_type_rows.out"
    writeIORef content . BL.cycle $ BL.fromStrict bs

    let handler dm = case dm of
            DataMessage _ -> modifyIORef' counter (+1)
            _             -> pure ()
        newChunk preBs = do
            b <- readIORef content
            let (nb, rest) = BL.splitAt 4096 b
            writeIORef content rest
            let res = preBs <> BL.toStrict nb
            res `seq` pure res
    tid <- forkIO . forever $ loopExtractDataRows newChunk handler
    threadDelay $ durationSeconds * 1000 * 1000
    killThread tid
    s <- readIORef counter
    putStrLn $ "Data messages parsed per second: "
            ++ show (s `div` fromIntegral durationSeconds)
  where
    durationSeconds :: Int
    durationSeconds = 10
#endif

getEnvDef :: String -> String -> IO String
getEnvDef name val = fromMaybe val <$> lookupEnv name

instance (NFData a1, NFData a2, NFData a3, NFData a4, NFData a5, NFData a6, NFData a7, NFData a8, NFData a9, NFData a10, NFData a11, NFData a12, NFData a13, NFData a14, NFData a15) =>
         NFData ((,,,,,,,,,,,,,,) a1 a2 a3 a4 a5 a6 a7 a8 a9 a10 a11 a12 a13 a14 a15) where
  rnf (x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,x15) = rnf x1 `seq` rnf x2 `seq` rnf x3 `seq` rnf x4 `seq` rnf x5 `seq` rnf x6 `seq` rnf x7 `seq` rnf x8 `seq` rnf x9 `seq` rnf x10 `seq` rnf x11 `seq` rnf x12 `seq` rnf x13 `seq` rnf x14 `seq` rnf x15

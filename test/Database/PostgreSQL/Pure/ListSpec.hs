{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}

module Database.PostgreSQL.Pure.ListSpec (spec) where

import           Database.PostgreSQL.Pure.List
import qualified Database.PostgreSQL.Pure.Oid     as Oid

import           Test.Hspec
import           Test.Hspec.Core.Hooks.Extra

import           Control.Monad                    (void)
import qualified Data.Attoparsec.ByteString       as AP
import qualified Data.Attoparsec.ByteString.Char8 as AP
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BSC
import qualified Data.ByteString.UTF8             as BSU
import           Data.Default.Class               (Default (def))
import           Data.Int                         (Int32)
import           Data.Maybe                       (fromMaybe)
import           Data.Tuple.Only                  (Only (Only))
import qualified Network.Socket                   as NS
import           System.Environment               (lookupEnv)
import           Text.Read                        (readMaybe)

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

data Env =
  Env
    { hostString :: String
    , portString :: String
    , host       :: Maybe NS.HostAddress
    , port       :: Maybe NS.PortNumber
    , user       :: String
    , password   :: String
    , database   :: String
    }

spec :: Spec
spec = do
  beforeAll
    ( do
        hostString <- getEnvDef "PURE_HOST" "127.0.0.1"
        portString <- getEnvDef "PURE_PORT" "5432"
        let
          host = fromEitherToMaybe $ parseHostIPv4 $ BSC.pack hostString
          port = readMaybe portString :: Maybe NS.PortNumber
        user <- getEnvDef "PURE_USER" "postgres"
        password <- getEnvDef "PURE_PASSWORD" ""
        database <- getEnvDef "PURE_DATABASE" "postgres"
        pure Env { hostString, portString, host, port, user, password, database }
    )
    $ do
        describe "connection" $ do
          describe "connect/disconnect" $ do
            it "resolved address" $ \Env { host, port, user, password, database } -> do
              case (host, port) of
                (Just host, Just port) -> do
                  let
                    config =
                      def
                        { address = AddressResolved $ NS.SockAddrInet port host
                        , user
                        , password
                        , database
                        }
                  conn <- connect config
                  disconnect conn
                _ -> pendingWith "the given host and port are not resolved"

            it "not-resolved address" $ \Env { hostString, portString, user, password, database } -> do
              let
                config =
                  def
                    { address = AddressNotResolved hostString portString
                    , user
                    , password
                    , database
                    }
              conn <- connect config
              disconnect conn

          it "withConnection" $ \Env { hostString, portString, user, password, database } -> do
            let
              config =
                def
                  { address = AddressNotResolved hostString portString
                  , user
                  , password
                  , database
                  }
            withConnection config (const $ pure ())

        beforeAllWith
          ( \Env { hostString, portString, user, password, database } -> do
              let
                config =
                  def
                    { address = AddressNotResolved hostString portString
                    , user
                    , password
                    , database
                    }
              connect config
          )
          $ afterAll
              disconnect
              $ do
                  describe "CREATE TABLE/DROP TABLE" $ do
                    it "hints, hints" $ \conn -> do
                      let
                        e0 = execute 0 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "CREATE TABLE test (value INT NOT NULL)" (Right ([], []))
                        e1 = execute 0 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "DROP TABLE IF EXISTS test" (Right ([], []))
                      (((_, _, e0, _), (_, _, e1, _)), ts) <- sync conn (e0, e1)
                      ts `shouldBe` Idle
                      result e0 `shouldBe` ExecuteComplete CreateTableTag
                      records e0 `shouldBe` ([] :: [()])
                      result e1 `shouldBe` ExecuteComplete DropTableTag
                      records e1 `shouldBe` ([] :: [()])

                    it "no hints, no hints" $ \conn -> do
                      let
                        e0 = execute 0 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "CREATE TABLE test (value INT NOT NULL)" (Left (0, 0))
                        e1 = execute 0 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "DROP TABLE IF EXISTS test" (Left (0, 0))
                      (((_, _, e0, _), (_, _, e1, _)), ts) <- sync conn (e0, e1)
                      ts `shouldBe` Idle
                      result e0 `shouldBe` ExecuteComplete CreateTableTag
                      records e0 `shouldBe` ([] :: [()])
                      result e1 `shouldBe` ExecuteComplete DropTableTag
                      records e1 `shouldBe` ([] :: [()])

                  beforeWith
                    ( \conn -> do
                         let
                           e0 = execute 0 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "CREATE TABLE test (value INT NOT NULL)" (Right ([], []))
                           e1 = execute 0 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "INSERT INTO test (value) VALUES (0), (1), (2)" (Right ([], []))
                         (((_, _, e0, _), (_, _, e1, _)), ts) <- sync conn (e0, e1)
                         ts `shouldBe` Idle
                         result e0 `shouldBe` ExecuteComplete CreateTableTag
                         records e0 `shouldBe` ([] :: [()])
                         result e1 `shouldSatisfy` \(ExecuteComplete (InsertTag _ 3)) -> True
                         records e1 `shouldBe` ([] :: [()])
                         pure conn
                    )
                    $ after
                        ( \conn -> do
                            let
                              e = execute 0 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "DROP TABLE IF EXISTS test" (Right ([], []))
                            ((_, _, e, _), ts) <- sync conn e
                            ts `shouldBe` Idle
                            result e `shouldBe` ExecuteComplete DropTableTag
                            records e `shouldBe` ([] :: [()])
                        )
                        $ do
                            describe "table: test (value INT NOT NULL)" $ do
                              it "DELETE FROM test WHERE value = 0" $ \conn -> do
                                let
                                  e = execute 0 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "DELETE FROM test WHERE value = 0" (Right ([], []))
                                ((_, _, e, _), ts) <- sync conn e
                                ts `shouldBe` Idle
                                result e `shouldBe` ExecuteComplete (DeleteTag 1)
                                records e `shouldBe` ([] :: [()])

                              it "UPDATE test SET value = 10 WHERE value = 1" $ \conn -> do
                                let
                                  e = execute 0 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "UPDATE test SET value = 10 WHERE value = 1" (Right ([], []))
                                ((_, _, e, _), ts) <- sync conn e
                                ts `shouldBe` Idle
                                result e `shouldBe` ExecuteComplete (UpdateTag 1)
                                records e `shouldBe` ([] :: [()])

                              it "SELECT value FROM test ORDER BY value (get all)" $ \conn -> do
                                let
                                  e = execute 0 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "SELECT value FROM test ORDER BY value" (Right ([], [Oid.int4]))
                                ((_, _, e, _), ts) <- sync conn e
                                ts `shouldBe` Idle
                                result e `shouldBe` ExecuteComplete (SelectTag 3)
                                records e `shouldBe` ([Only 0, Only 1, Only 2] :: [Only Int])

                              it "SELECT value FROM test ORDER BY value (get a part)" $ \conn -> do
                                let
                                  e = execute 1 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "SELECT value FROM test ORDER BY value" (Right ([], [Oid.int4]))
                                ((_, _, e, _), ts) <- sync conn e
                                ts `shouldBe` Idle
                                result e `shouldBe` ExecuteSuspended
                                records e `shouldBe` ([Only 0] :: [Only Int])

                              it "SELECT value FROM test ORDER BY value (reuse portal)" $ \conn -> do
                                let
                                  p = forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "SELECT value FROM test ORDER BY value" (Right ([], [Oid.int4]))
                                  e = execute 2 (pure . BSU.toString) p
                                (_, p, e, _) <- flush conn e
                                result e `shouldBe` ExecuteSuspended
                                records e `shouldBe` ([Only 0, Only 1] :: [Only Int])
                                let
                                  e = execute 1 (pure . BSU.toString) p
                                (_, _, e, _) <- flush conn e
                                result e `shouldBe` ExecuteSuspended
                                records e `shouldBe` ([Only 2] :: [Only Int])
                                let
                                  e = execute 1 (pure . BSU.toString) p
                                ((_, _, e, _), ts) <- sync conn e
                                ts `shouldBe` Idle
                                result e `shouldBe` ExecuteComplete (SelectTag 0)
                                records e `shouldBe` ([] :: [Only Int])

                              it "SELECT value FROM test WHERE value = $1 (reuse prepared statement)" $ \conn -> do
                                let
                                  ps = parse "" "SELECT value FROM test WHERE value = $1" (Right ([Oid.int4], [Oid.int4]))
                                  e = execute 0 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) (Only (0 :: Int32)) ps
                                ((ps, _, e, _), ts) <- sync conn e
                                ts `shouldBe` Idle
                                result e `shouldBe` ExecuteComplete (SelectTag 1)
                                records e `shouldBe` [Only (0 :: Int)]
                                let
                                  e = execute 0 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) (Only (2 :: Int32)) ps
                                ((_, _, e, _), ts) <- sync conn e
                                ts `shouldBe` Idle
                                result e `shouldBe` ExecuteComplete (SelectTag 1)
                                records e `shouldBe` [Only (2 :: Int)]

                              it "BEGIN/ROLLBACK" $ \conn -> do
                                ((_, _, e, _), ts) <- sync conn begin
                                ts `shouldBe` Block
                                result e `shouldBe` ExecuteComplete BeginTag
                                records e `shouldBe` []
                                ((_, _, e, _), ts) <- sync conn rollback
                                ts `shouldBe` Idle
                                result e `shouldBe` ExecuteComplete RollbackTag
                                records e `shouldBe` []

                              it "BEGIN/COMMIT" $ \conn -> do
                                ((_, _, e, _), ts) <- sync conn begin
                                ts `shouldBe` Block
                                result e `shouldBe` ExecuteComplete BeginTag
                                records e `shouldBe` []
                                ((_, _, e, _), ts) <- sync conn commit
                                ts `shouldBe` Idle
                                result e `shouldBe` ExecuteComplete CommitTag
                                records e `shouldBe` []

                            describe "invalid SQL" $ do
                              it "parse sync" $ \conn -> do
                                let
                                  ps = parse "" "INVALID SQL" (Right ([], []))
                                sync conn ps `shouldThrow` (\ErrorResponse {} -> True)

                              it "parse flush" $ \conn -> do
                                let
                                  ps = parse "" "INVALID SQL" (Right ([], []))
                                flush conn ps `shouldThrow` (\ErrorResponse {} -> True)

                            describe "invalid parameter type" $ do
                              it "parse sync" $ \conn -> do
                                let
                                  ps = parse "" "SELECT value FROM test WHERE value = $1" (Right ([Oid.varchar], [Oid.int4]))
                                void $ sync conn ps `shouldThrow` (\ErrorResponse {} -> True)

                              it "parse flush" $ \conn -> do
                                let
                                  ps = parse "" "SELECT value FROM test WHERE value = $1" (Right ([Oid.varchar], [Oid.int4]))
                                void $ flush conn ps `shouldThrow` (\ErrorResponse {} -> True)

                            describe "invalid result type" $ do
                              it "parse/bind sync" $ \conn -> do
                                let
                                  p = forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "SELECT value FROM test ORDER BY value LIMIT 1" (Right ([], [Oid.varchar]))
                                ((_, _), ts) <- sync conn p
                                ts `shouldBe` Idle

                              it "parse/bind flush" $ \conn -> do
                                let
                                  p = forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "SELECT value FROM test ORDER BY value LIMIT 1" (Right ([], [Oid.varchar]))
                                void $ flush conn p

                              it "parse/bind/execute sync" $ \conn -> do
                                let
                                  e = execute @_ @(Only Int) 0 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "SELECT value FROM test ORDER BY value LIMIT 1" (Right ([], [Oid.varchar]))
                                sync conn e `shouldThrow` (\ResponseParsingFailed {} -> True)

                              it "parse/bind/execute flush" $ \conn -> do
                                let
                                  e = execute @_ @(Only Int) 0 (pure . BSU.toString) $ forceRight $ bind "" BinaryFormat BinaryFormat (parameters conn) (pure . BSU.fromString) () $ parse "" "SELECT value FROM test ORDER BY value LIMIT 1" (Right ([], [Oid.varchar]))
                                flush conn e `shouldThrow` (\ResponseParsingFailed {} -> True)

getEnvDef :: String -> String -> IO String
getEnvDef name value = fromMaybe value <$> lookupEnv name

parseHostIPv4 :: BS.ByteString -> Either String NS.HostAddress
parseHostIPv4 =
  AP.parseOnly ipv4Parser
  where
    ipv4Parser = do
      p1 <- AP.decimal
      void $ AP.char '.'
      p2 <- AP.decimal
      void $ AP.char '.'
      p3 <- AP.decimal
      void $ AP.char '.'
      p4 <- AP.decimal
      pure $ NS.tupleToHostAddress (p1, p2, p3, p4)

fromEitherToMaybe :: Either a b -> Maybe b
fromEitherToMaybe (Left _)  = Nothing
fromEitherToMaybe (Right b) = Just b

forceRight :: Either String a -> a
forceRight (Right a) = a
forceRight (Left e)  = error $ "forceRight: " <> e

{-# LANGUAGE NamedFieldPuns   #-}
{-# LANGUAGE TypeApplications #-}

module Database.HDBC.PostgreSQL.PureSpec (spec) where

import           Database.HDBC
import           Database.HDBC.PostgreSQL.Pure

import           Test.Hspec

import           Control.Exception.Safe        (try)
import           Control.Monad                 (void)
import           Data.Default.Class            (Default (def))
import           Data.Maybe                    (fromMaybe)
import           System.Environment            (lookupEnv)

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication" :: String) #-}

spec :: Spec
spec = do
  beforeAll
    ( do
        host <- getEnvDef "PURE_HOST" "127.0.0.1"
        port <- getEnvDef "PURE_PORT" "5432"
        user <- getEnvDef "PURE_USER" "postgres"
        password <- getEnvDef "PURE_PASSWORD" ""
        database <- getEnvDef "PURE_DATABASE" "postgres"
        let
          config =
            def
              { address = AddressNotResolved host port
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
              it "prepare/execute" $ \conn -> do
                statement <- prepare conn "CREATE TABLE test (value INT NOT NULL)"
                n <- execute statement []
                n `shouldBe` 0
                statement <- prepare conn "DROP TABLE IF EXISTS test"
                n <- execute statement []
                n `shouldBe` 0
                commit conn
                pure ()

              it "run" $ \conn -> do
                run conn "CREATE TABLE test (value INT NOT NULL)" [] `shouldReturn` 0
                run conn "DROP TABLE IF EXISTS test" [] `shouldReturn` 0
                commit conn

              it "runRaw" $ \conn -> do
                let
                  query =
                    "CREATE TABLE test (value INT NOT NULL);\
                    \DROP TABLE IF EXISTS test"
                runRaw conn query
                commit conn

            beforeWith
              ( \conn -> do
                  let
                    query =
                      "CREATE TABLE test (value INT NOT NULL);\
                      \INSERT INTO test (value) VALUES (0), (1), (2)"
                  runRaw conn query
                  commit conn
                  pure conn
              )
              $ after
                  ( \conn -> do
                      void $ try @IO @SqlError $ do
                        runRaw conn "DROP TABLE IF EXISTS test"
                        commit conn
                  )
                  $ do
                      describe "table: test (value INT NOT NULL)" $ do
                        it "DELETE FROM test WHERE value = 0" $ \conn -> do
                          run conn "DELETE FROM test WHERE value = 0" [] `shouldReturn` 1
                          commit conn

                        it "UPDATE test SET value = 10 WHERE value = 1" $ \conn -> do
                          run conn "UPDATE test SET value = 10 WHERE value = 1" [] `shouldReturn` 1
                          commit conn

                        it "SELECT value FROM test ORDER BY value (reuse portal)" $ \conn -> do
                          s <- prepare conn "SELECT value FROM test ORDER BY value"
                          executeRaw s
                          fetchRow s `shouldReturn` Just [SqlInt32 0]
                          fetchRow s `shouldReturn` Just [SqlInt32 1]
                          fetchRow s `shouldReturn` Just [SqlInt32 2]
                          fetchRow s `shouldReturn` Nothing
                          finish s

                        it "SELECT value FROM test WHERE value = $1 (reuse prepared statement)" $ \conn -> do
                          s <- prepare conn "SELECT value FROM test WHERE value = ?"
                          void $ execute s [SqlInt32 0]
                          fetchRow s `shouldReturn` Just [SqlInt32 0]
                          void $ execute s [SqlInt32 1]
                          fetchRow s `shouldReturn` Just [SqlInt32 1]
                          void $ execute s [SqlInt32 2]
                          fetchRow s `shouldReturn` Just [SqlInt32 2]
                          finish s

                        it "BEGIN/ROLLBACK" $ \conn -> do
                          begin conn
                          rollback conn

                        it "BEGIN/COMMIT" $ \conn -> do
                          begin conn
                          commit conn

getEnvDef :: String -> String -> IO String
getEnvDef name value = fromMaybe value <$> lookupEnv name

{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.PostgreSQL.Pure.Internal.Connection
  ( connect
  , disconnect
  , withConnection
  ) where

import qualified Database.PostgreSQL.Pure.Internal.Builder   as Builder
import           Database.PostgreSQL.Pure.Internal.Data      (Address (AddressNotResolved, AddressResolved),
                                                              AuthenticationMD5Password (AuthenticationMD5Password),
                                                              AuthenticationResponse (AuthenticationMD5PasswordResponse, AuthenticationOkResponse, AuthenticationCleartextPasswordResponse),
                                                              BackendKey, BackendKeyData (BackendKeyData),
                                                              BackendParameters, Buffer (Buffer),
                                                              Config (Config, address, database, password, receptionBufferSize, sendingBufferSize, user),
                                                              Connection (Connection, config, receptionBuffer, sendingBuffer, socket),
                                                              ParameterStatus (ParameterStatus), Pid,
                                                              ReadyForQuery (ReadyForQuery), Salt,
                                                              TransactionState (Idle))
import qualified Database.PostgreSQL.Pure.Internal.Exception as Exception
import qualified Database.PostgreSQL.Pure.Internal.Parser    as Parser
import           Database.PostgreSQL.Pure.Internal.SocketIO  (SocketIO, buildAndSend, receive, runSocketIO, send)

import           Control.Exception.Safe                      (assert, bracket)
import           Control.Monad                               (void)
import           Control.Monad.Reader                        (ask)
import qualified Data.Attoparsec.ByteString                  as AP
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Base16                      as B16
import qualified Data.ByteString.Internal                    as BSI
import qualified Data.ByteString.UTF8                        as BSU
import qualified Data.Map.Strict                             as Map
import qualified Network.Socket                              as NS

#ifdef PURE_MD5
import qualified Data.Digest.Pure.MD5                        as MD5
#else
import qualified Crypto.Hash.MD5                             as MD5
#endif

-- | Bracket function for a connection.
withConnection :: Config -> (Connection -> IO a) -> IO a
withConnection config@Config { address } f =
  Exception.convert $ do
    addr <-
      case address of
        AddressResolved a      -> pure $ addrInfo a
        AddressNotResolved h s -> resolve h s
    bracket (open addr) NS.close $ \sock -> do
      conn <- connect' sock config
      f conn

-- | To connect to the server.
connect :: Config -> IO Connection
connect config@Config { address } =
  Exception.convert $ do
    addr <-
      case address of
        AddressResolved a      -> pure $ addrInfo a
        AddressNotResolved h s -> resolve h s
    sock <- open addr
    connect' sock config

connect' :: NS.Socket -> Config -> IO Connection
connect' sock config@Config { sendingBufferSize, receptionBufferSize } = do
    sBuff <- flip Buffer sendingBufferSize <$> BSI.mallocByteString sendingBufferSize
    rBuff <- flip Buffer receptionBufferSize <$> BSI.mallocByteString receptionBufferSize
    runSocketIO sock sBuff rBuff config initializeConnection

-- | To disconnect to the server.
disconnect :: Connection -> IO ()
disconnect Connection { socket, sendingBuffer, receptionBuffer, config } =
  Exception.convert $ do
    runSocketIO socket sendingBuffer receptionBuffer config terminate
    NS.close socket

addrInfoHints :: NS.AddrInfo
addrInfoHints =
  NS.defaultHints
    { NS.addrSocketType = NS.Stream
    , NS.addrProtocol = 6 -- TCP
    , NS.addrFlags = [NS.AI_ADDRCONFIG]
    }

addrInfo :: NS.SockAddr -> NS.AddrInfo
addrInfo address =
  addrInfoHints
    { NS.addrAddress = address
    , NS.addrFamily =
        case address of
          NS.SockAddrInet {}  -> NS.AF_INET
          NS.SockAddrInet6 {} -> NS.AF_INET6
          NS.SockAddrUnix {}  -> NS.AF_UNIX
#if !MIN_VERSION_network(3,0,0)
          _                   -> NS.AF_UNSPEC
#endif
    }

resolve :: NS.HostName -> NS.ServiceName -> IO NS.AddrInfo
resolve host service = do
  addrs <- NS.getAddrInfo (Just addrInfoHints) (Just host) (Just service)
  case addrs of
    addr:_ -> return addr
    []     -> Exception.cantReachHere

open :: NS.AddrInfo -> IO NS.Socket
open addr = do
  sock <- NS.socket (NS.addrFamily addr) (NS.addrSocketType addr) (NS.addrProtocol addr)
  NS.connect sock $ NS.addrAddress addr
  return sock

initializeConnection :: SocketIO Connection
initializeConnection = do
  response <- startup
  (bps, pid, bk) <- authenticate response
  (sock, sBuff, rBuff, config) <- ask
  pure $ Connection sock pid bk bps sBuff rBuff config

startup :: SocketIO AuthenticationResponse
startup = do
  (_, _, _, Config { user, database }) <- ask
  buildAndSend $ Builder.startup user database
  receive Parser.authentication

authenticate :: AuthenticationResponse -> SocketIO (BackendParameters, Pid, BackendKey)
authenticate response = do
  (_, _, _, Config { user, password }) <- ask
  case response of
    AuthenticationOkResponse                                           -> pure ()
    AuthenticationCleartextPasswordResponse                            -> auth $ BSU.fromString password
    AuthenticationMD5PasswordResponse (AuthenticationMD5Password salt) -> auth $ hashMD5 user password salt
  (bps, pid, bk) <-
    receive $ do
      bps <- Map.fromList . ((\(ParameterStatus k v) -> (k, v)) <$>) <$> AP.many' Parser.parameterStatus
      BackendKeyData pid bk <- Parser.backendKeyData
      ReadyForQuery ts <- Parser.readyForQuery
      assert (ts == Idle) $ pure (bps, pid, bk)
  pure (bps, pid, bk)
  where
    auth pw = do
      buildAndSend $ Builder.password pw
      void $ receive Parser.authenticationOk

terminate :: SocketIO ()
terminate = send Builder.terminate

hashMD5 :: String -> String -> Salt -> BS.ByteString
hashMD5 user password salt =
  let
    user' = BSU.fromString user
    password' = BSU.fromString password
#ifdef PURE_MD5
    hash = B16.encode . MD5.md5DigestBytes . MD5.hash'
#else
    hash = B16.encode . MD5.hash
#endif
  in
    "md5" <> hash (hash (password' <> user') <> salt)

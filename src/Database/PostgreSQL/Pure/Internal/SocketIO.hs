{-# LANGUAGE CPP #-}

module Database.PostgreSQL.Pure.Internal.SocketIO
  ( SocketIO
  , runSocketIO
  , send
  , buildAndSend
  , receive
  ) where

import           Database.PostgreSQL.Pure.Internal.Data      (Buffer (Buffer), Carry, Config, Error (Error))
import qualified Database.PostgreSQL.Pure.Internal.Exception as Exception
import qualified Database.PostgreSQL.Pure.Internal.Parser    as Parser

import           Control.Concurrent                          (yield)
import           Control.Exception.Safe                      (throw, try, tryJust)
import           Control.Monad                               (guard, unless)
import           Control.Monad.IO.Class                      (liftIO)
import           Control.Monad.Reader                        (ReaderT, ask, runReaderT)
import           Control.Monad.State.Strict                  (StateT, get, put, runStateT)
import qualified Data.Attoparsec.ByteString                  as AP
import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Builder                     as BSB
import qualified Data.ByteString.Builder.Extra               as BSB
import qualified Data.ByteString.Internal                    as BSI
import           Data.List                                   (intercalate)
import           Data.Word                                   (Word8)
import           Foreign                                     (ForeignPtr, Ptr, withForeignPtr)
import qualified Network.Socket                              as NS
import qualified Network.Socket.ByteString                   as NSB
import           System.IO.Error                             (isEOFError)

type SocketIO = StateT Carry (ReaderT (NS.Socket, Buffer, Buffer, Config) IO)

runSocketIO :: NS.Socket -> Buffer -> Buffer -> Config -> SocketIO a -> IO a
runSocketIO s sb rb c m =
  flip runReaderT (s, sb, rb, c) $ do
    (a, carry) <- runStateT m BS.empty
    unless (BS.null carry) $ throw $ Exception.InternalExtraData carry
    pure a

send :: BS.ByteString -> SocketIO ()
send message = do
  (sock, _, _, _) <- ask
  liftIO $ do
    NSB.sendAll sock message
    yield

buildAndSend :: BSB.Builder -> SocketIO ()
buildAndSend builder = do
  (_, Buffer fp len, _, _) <- ask
  go fp len $ BSB.runBuilder builder
  where
    go :: ForeignPtr Word8 -> Int -> BSB.BufferWriter -> SocketIO ()
    go bfp blen writer = do
      (wc, next) <- liftIO $ withForeignPtr bfp $ \ptr -> writer ptr blen
      send $ BSI.PS bfp 0 wc
      case next of
        BSB.Done -> pure ()
        BSB.More newLen w
          | newLen <= blen -> go bfp blen w
          | otherwise -> do
            newFPtr <- liftIO $ BSI.mallocByteString newLen
            go newFPtr newLen w
        BSB.Chunk bs w -> do
          send bs
          go bfp blen w

recvAndParse :: NS.Socket -> Buffer -> Carry -> AP.Parser response -> IO (response, Carry)
recvAndParse sock (Buffer bfptr blen) carry parser =
  withForeignPtr bfptr $ \bptr -> do
    let
      recv :: IO BS.ByteString
      recv = do
        len <- recvBuf sock bptr blen
        case len of
          0 -> pure BS.empty -- EOF
          _ -> pure $ BS.copy $ BSI.PS bfptr 0 len
    result <- AP.parseWith recv parser carry
    case result of
      AP.Done rest response -> pure (response, rest)
      AP.Fail rest [] msg      -> throw $ Exception.InternalResponseParsingFailed msg rest
      AP.Fail rest ctxs msg    -> throw $ Exception.InternalResponseParsingFailed (intercalate " > " ctxs <> ": " <> msg) rest
      AP.Partial _          -> Exception.cantReachHere

receiveJust :: AP.Parser response -> SocketIO response
receiveJust parser = do
  carry <- get
  (sock, _, buff, _) <- ask
  (response, carry') <- liftIO $ recvAndParse sock buff carry parser
  put carry'
  pure response

receive :: AP.Parser response -> SocketIO response
receive parser = do
  r <- try $ receiveJust parser
  case r of
    Right r             -> pure r
    Left e@(Exception.InternalResponseParsingFailed _ raw) -> do
      let
        r = AP.parseOnly Parser.skipUntilError raw
      throw $
        case r of
          Right (Error fields) -> Exception.InternalErrorResponse fields Nothing
          Left _               -> e
    Left e -> throw e

-- Before network 3.0.0.0, recvBuf raises error on EOF. Otherwise it returns 0 on EOF.
recvBuf :: NS.Socket -> Ptr Word8 -> Int -> IO Int
#if MIN_VERSION_network(3, 0, 0)
recvBuf s ptr nbytes = NS.recvBuf s ptr nBytes
#else
recvBuf s ptr nbytes = do
  r <- tryJust (guard . isEOFError) $ NS.recvBuf s ptr nbytes
  case r of
    Left _  -> pure 0
    Right l -> pure l
#endif

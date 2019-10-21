{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedLabels      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module Database.PostgreSQL.Pure.Internal.Query
  ( -- * Extended Query
    parse
  , Bind (..)
  , Execute (..)
  , flush
  , sync
  , Message (..)
  , Close (..)
    -- * Transaction
  , begin
  , commit
  , rollback
  ) where

import qualified Database.PostgreSQL.Pure.Internal.Builder   as Builder
import           Database.PostgreSQL.Pure.Internal.Data      (BackendParameters,
                                                              BindParameterFormatCodes (BindParameterFormatCodesAll),
                                                              BindResultFormatCodes (BindResultFormatCodesEach),
                                                              CloseProcedure (CloseProcedure),
                                                              ColumnInfo (ColumnInfo, formatCode),
                                                              CommandComplete (CommandComplete),
                                                              Connection (Connection, config, receptionBuffer, sendingBuffer, socket),
                                                              DataRow (DataRow), ErrorFields,
                                                              ExecuteResult (ExecuteComplete, ExecuteEmptyQuery, ExecuteSuspended),
                                                              Executed (Executed),
                                                              ExecutedProcedure (ExecutedProcedure),
                                                              FormatCode (BinaryFormat), FromRecord, MessageResult,
                                                              Notice (Notice), Oid,
                                                              ParameterDescription (ParameterDescription),
                                                              Portal (Portal), PortalName,
                                                              PortalProcedure (PortalProcedure),
                                                              PreparedStatement (PreparedStatement),
                                                              PreparedStatementName,
                                                              PreparedStatementProcedure (PreparedStatementProcedure),
                                                              Query, ReadyForQuery (ReadyForQuery),
                                                              RowDescription (RowDescription), StringDecoder,
                                                              StringEncoder, ToRecord (toRecord), TransactionState,
                                                              TypeLength (FixedLength))
import qualified Database.PostgreSQL.Pure.Internal.Data      as Data
import qualified Database.PostgreSQL.Pure.Internal.Exception as Exception
import           Database.PostgreSQL.Pure.Internal.IsLabel   ()
import qualified Database.PostgreSQL.Pure.Internal.Parser    as Parser
import           Database.PostgreSQL.Pure.Internal.SocketIO  (buildAndSend, receive, runSocketIO, send)

import           Control.Applicative                         ((<|>))
import           Control.Exception.Safe                      (throw, try)
import           Control.Monad                               (void, when)
import           Control.Monad.Fail                          (MonadFail)
import           Control.Monad.State.Strict                  (put)
import qualified Data.Attoparsec.ByteString                  as AP
import qualified Data.Attoparsec.Combinator                  as AP
import qualified Data.ByteString.Builder                     as BSB
import qualified Data.ByteString.Char8                       as BSC
import           Data.Functor                                (($>))
import           Data.List                                   (genericLength)
import           GHC.OverloadedLabels                        (IsLabel)

-- | To get the procedure to build the message of parsing SQL query and to parse its response.
parse
  :: PreparedStatementName -- ^ A new name of prepared statement.
  -> Query -- ^ SQL whose placeoholder style is dollar style.
  -> Either (Word, Word) ([Oid], [Oid]) -- ^ A pair of the number of columns of the parameter and the result,
                                        -- or a pair of the list of OIDs of the parameter and the result.
                                        -- On 'Left' an additional pair of a request and a resposne is necessary.
  -> PreparedStatementProcedure
parse name query (Left (parameterLength, resultLength)) = parse' name query parameterLength resultLength Nothing
parse name query (Right oids@(parameterOids, resultOids)) = parse' name query (genericLength parameterOids) (genericLength resultOids) (Just oids)

parse' :: PreparedStatementName -> Query -> Word -> Word -> Maybe ([Oid], [Oid]) -> PreparedStatementProcedure
parse' name query parameterLength resultLength oids =
  let
    inaneColumnInfo oid = ColumnInfo "" 0 0 oid (FixedLength 0) 0 BinaryFormat
    parameterOids = fst <$> oids
    builder =
      case oids of
        Just (parameterOids, _) -> Builder.parse name query parameterOids
        _                       -> Builder.parse name query [] <> Builder.describePreparedStatement name
    parser = do
      Parser.parseComplete
      (parameterOids, resultInfos) <-
        case oids of
          Just (parameterOids, resultOids) -> pure (parameterOids, inaneColumnInfo <$> resultOids)
          _ -> do
            ParameterDescription parameterOids <- Parser.parameterDescription
            resultInfos <-
              AP.choice
                [ do
                    RowDescription infos <- Parser.rowDescription
                    pure infos
                , Parser.noData $> []
                ]
            pure (parameterOids, resultInfos)
      pure $ PreparedStatement name parameterOids resultInfos
  in PreparedStatementProcedure name parameterLength resultLength parameterOids builder parser

-- | This means that @ps@ is a objective of 'bind'.
class Bind ps where
  -- | To get the procedure to build the message of binding the parameter and to parse its response.
  bind
    :: (ToRecord param, MonadFail m)
    => PortalName -- ^ A new name of portal.
    -> FormatCode -- ^ Binary format or text format for the parameter.
    -> FormatCode -- ^ Binary format or text format for the results.
    -> BackendParameters -- ^ The set of the server parameters.
    -> StringEncoder -- ^ How to encode strings.
    -> param -- ^ Parameter for this query.
    -> ps -- ^ Prepared statement.
    -> m PortalProcedure

instance Bind PreparedStatement where
  bind name parameterFormat resultFormat backendParams encode parameters ps@(PreparedStatement psName psParameterOids psResultInfos) = do
    record <- toRecord backendParams encode (Just psParameterOids) (replicate (length psParameterOids) parameterFormat) parameters
    let
      builder = Builder.bind name psName (BindParameterFormatCodesAll parameterFormat) record (BindResultFormatCodesEach $ replicate (length psResultInfos) resultFormat)
      parser = do
        Parser.bindComplete
        pure (ps, Portal name ((\i -> i { formatCode = resultFormat }) <$> psResultInfos) ps)
    pure $ PortalProcedure name resultFormat builder parser

instance Bind PreparedStatementProcedure where
  bind name parameterFormat resultFormat backendParams encode parameters (PreparedStatementProcedure psName psParameterLength psResultLength psParameterOids psBuilder psParser) = do
    record <- toRecord backendParams encode psParameterOids (replicate (fromIntegral psParameterLength) parameterFormat) parameters
    let
      builder =
        psBuilder
        <> Builder.bind name psName (BindParameterFormatCodesAll parameterFormat) record (BindResultFormatCodesEach $ replicate (fromIntegral psResultLength) resultFormat)
      parser = do
        ps@PreparedStatement { resultInfos } <- psParser
        Parser.bindComplete
        pure (ps, Portal name ((\i -> i { formatCode = resultFormat }) <$> resultInfos) ps)
    pure $ PortalProcedure name resultFormat builder parser

-- | This means that @p@ is a objective of 'execute'.
class Execute p where
  -- | To get the procedure to build the message of execution and to parse its response.
  execute
    :: FromRecord result
    => Word -- ^ How many records to get. “0” means unlimited.
    -> StringDecoder -- ^ How to decode strings.
    -> p -- ^ Portal.
    -> ExecutedProcedure result

instance Execute Portal where
  execute rowLimit decode p@(Portal pName pInfos ps@PreparedStatement {}) =
    let
      builder = Builder.execute pName $ fromIntegral rowLimit
      parser = executeParser ps p pInfos decode
    in ExecutedProcedure builder parser

instance Execute PortalProcedure where
  execute rowLimit decode (PortalProcedure pName pFormat pBuilder pParser) =
    let
      builder = pBuilder <> Builder.execute pName (fromIntegral rowLimit)
      parser = do
        (ps@(PreparedStatement _ _ psInfos), p) <- pParser
        executeParser ps p ((\i -> i { formatCode = pFormat }) <$> psInfos) decode
    in ExecutedProcedure builder parser

executeParser :: forall r. FromRecord r => PreparedStatement -> Portal -> [ColumnInfo] -> StringDecoder -> AP.Parser (PreparedStatement, Portal, Executed r, Maybe ErrorFields)
executeParser ps p infos decode = do
  records <- ((\(DataRow d) -> d) <$>) <$> AP.many' (Parser.dataRow decode infos)
  when (null records) $ do
    -- detect whether no "data row" responses or value parsing failure
    r <- AP.option False $ AP.lookAhead Parser.dataRowRaw >> pure True
    when r $ do
      -- get detailed error
      void (Parser.dataRow decode infos :: AP.Parser (DataRow r))
      fail "can't reach here"
  err <- AP.option Nothing $ (\(Notice err) -> Just err) <$> Parser.notice
  result <-
    ((\(CommandComplete tag) -> ExecuteComplete tag) <$> Parser.commandComplete)
    <|> (Parser.emptyQuery >> pure ExecuteEmptyQuery)
    <|> (Parser.portalSuspended >> pure ExecuteSuspended)
  pure (ps, p, Executed result records p, err)

-- | This means that @p@ is a objective of 'close'.
class Close p where
  -- | To build and send the “Close” message and to receive and parse its response.
  close :: p -> CloseProcedure

instance Close PreparedStatement where
  close p = CloseProcedure (Builder.closePreparedStatement $ #name p) Parser.closeComplete

instance Close Portal where
  close p = CloseProcedure (Builder.closePortal $ #name p) Parser.closeComplete

-- | This means than @r@ is a objective of 'flush' and 'sync'.
class Message m where
  builder :: m -> BSB.Builder
  default builder :: IsLabel "builder" (m -> BSB.Builder) => m -> BSB.Builder
  builder = #builder

  parser :: m -> AP.Parser (MessageResult m)
  default parser :: IsLabel "parser" (m -> AP.Parser (MessageResult m)) => m -> AP.Parser (MessageResult m)
  parser = #parser

instance Message PreparedStatementProcedure

instance Message PortalProcedure

instance Message (ExecutedProcedure r)

instance Message CloseProcedure

instance Message () where
  builder _ = mempty
  parser _ = pure ()

type instance MessageResult () = ()

instance (Message m0, Message m1) => Message (m0, m1) where
  builder (m0, m1) = builder m0 <> builder m1
  parser (m0, m1) = (,) <$> parser m0 <*> parser m1

type instance MessageResult (m0, m1) = (MessageResult m0, MessageResult m1)

instance (Message m0, Message m1, Message m2) => Message (m0, m1, m2) where
  builder (m0, m1, m2) = builder m0 <> builder m1 <> builder m2
  parser (m0, m1, m2) = (,,) <$> parser m0 <*> parser m1 <*> parser m2

type instance MessageResult (m0, m1, m2) = (MessageResult m0, MessageResult m1, MessageResult m2)

instance (Message m0, Message m1, Message m2, Message m3) => Message (m0, m1, m2, m3) where
  builder (m0, m1, m2, m3) = builder m0 <> builder m1 <> builder m2 <> builder m3
  parser (m0, m1, m2, m3) = (,,,) <$> parser m0 <*> parser m1 <*> parser m2 <*> parser m3

type instance MessageResult (m0, m1, m2, m3) = (MessageResult m0, MessageResult m1, MessageResult m2, MessageResult m3)

instance Message m => Message [m] where
  builder = mconcat . (builder <$>)
  parser = sequence . (parser <$>)

type instance MessageResult [m] = [MessageResult m]

-- | To build and send the given message and a “Flush” message and to receive and parse those responses.
flush :: Message m => Connection -> m -> IO (MessageResult m)
flush Connection { socket, sendingBuffer, receptionBuffer, config } m =
  Exception.convert $
    runSocketIO socket sendingBuffer receptionBuffer config $ do
      r <- try $ do
        buildAndSend $ builder m <> BSB.byteString Builder.flush
        receive $ parser m
      case r of
        Right r -> pure r
        Left (Exception.InternalErrorResponse fields _ _) -> do
          ReadyForQuery ts <- do
            put mempty
            send Builder.sync
            receive Parser.readyForQuery
          throw $ Exception.InternalErrorResponse fields (Just ts) mempty
        Left e -> throw e

-- | To build and send the given message and a “Sync” message and to receive and parse those responses.
sync :: Message m => Connection -> m -> IO (MessageResult m, TransactionState)
sync Connection { socket, sendingBuffer, receptionBuffer, config } m =
  Exception.convert $
    runSocketIO socket sendingBuffer receptionBuffer config $ do
      r <-
        try $ do
          buildAndSend $ builder m <> BSB.byteString Builder.sync
          (r, ReadyForQuery ts) <- receive $ (,) <$> parser m <*> Parser.readyForQuery
          pure (r, ts)
      case r of
        Right r -> pure r
        Left (Exception.InternalErrorResponse fields _ rest) -> do
          put rest
          ReadyForQuery ts <- receive Parser.readyForQuery
          throw $ Exception.InternalErrorResponse fields (Just ts) mempty
        Left e -> throw e

-- | To send @BEGIN@ SQL statement.
begin :: ExecutedProcedure ()
begin = transact "BEGIN"

-- | To send @COMMIT@ SQL statement.
commit :: ExecutedProcedure ()
commit = transact "COMMIT"

-- | To send @ROLLBACK@ SQL statement.
rollback :: ExecutedProcedure ()
rollback = transact "ROLLBACK"

transact :: Query -> ExecutedProcedure ()
transact q =
  let
    psProc = parse "" q (Right ([], []))
  in
    case bind "" BinaryFormat BinaryFormat mempty (pure . BSC.pack) () psProc of -- mempty (backend parameters) and BSC.pack (string encoder) are not used.
      Right pProc -> execute 1 (pure . BSC.unpack) pProc
      Left err    -> error err

{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Database.PostgreSQL.Pure.Internal.Data
  ( Connection (..)
  , Config (..)
  , ColumnInfo (..)
  , Response (..)
  , AuthenticationResponse (..)
  , AuthenticationMD5Password (..)
  , BackendKeyData (..)
  , CommandComplete (..)
  , DataRow (..)
  , DataRowRaw (..)
  , Error (..)
  , Notice (..)
  , ParameterStatus (..)
  , ReadyForQuery (..)
  , RowDescription (..)
  , ParameterDescription (..)
  , Debug (..)
  , ExecuteResult (..)
  , DescribeResult (..)
  , AttributeNumber
  , TypeModifier
  , FormatCode (..)
  , BindParameterFormatCodes (..)
  , BindResultFormatCodes (..)
  , TypeLength (..)
  , CommandTag (..)
  , ErrorFields (..)
  , TransactionState (..)
  , Buffer (..)
  , Carry
  , Salt
  , Address (..)
  , BackendParameters
  , Pid
  , BackendKey
  , Oid (..)
  , Raw (Null, Value)
  , Query (..)
  , PreparedStatement (..)
  , PreparedStatementProcedure (..)
  , PreparedStatementName (..)
  , Portal (..)
  , PortalProcedure (..)
  , PortalName (..)
  , Executed (..)
  , ExecutedProcedure (..)
  , CloseProcedure (..)
  , MessageResult
  , StringDecoder
  , StringEncoder
  , FromField (..)
  , FromRecord (..)
  , ToField (..)
  , ToRecord (..)
  , SqlIdentifier (..)
  , Pretty (..)
  ) where

import           Database.PostgreSQL.Pure.Oid (Oid (Oid))

import           Control.Applicative          ((<|>))
import qualified Data.Attoparsec.ByteString   as AP
import qualified Data.ByteString              as BS
import qualified Data.ByteString.Builder      as BSB
import qualified Data.ByteString.Short        as BSS
import qualified Data.ByteString.UTF8         as BSU
import           Data.Char                    (chr, isPrint, toLower)
import           Data.Default.Class           (Default (def))
import           Data.Int                     (Int16, Int32)
import           Data.Kind                    (Type)
import           Data.List                    (intercalate)
import           Data.Map.Strict              (Map)
import           Data.String                  (IsString)
import           Data.Word                    (Word8)
import           Foreign                      (ForeignPtr)
import           Hexdump                      (prettyHex, simpleHex)
import           Network.Socket               (Socket)
import qualified Network.Socket               as NS
import           Text.Read                    (Read (readPrec))
import qualified Text.Read                    as R
import qualified Text.Read.Lex                as R

#if !MIN_VERSION_base(4,13,0)
import           Control.Monad.Fail           (MonadFail)
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
data Config =
  Config
    { address             :: Address -- ^ Server address.
    , user                :: String -- ^ User name.
    , password            :: String -- ^ Password of user.
    , database            :: String -- ^ Database name.
    , sendingBufferSize   :: Int -- ^ The size of sending buffer in byte.
    , receptionBufferSize :: Int -- ^ The size of receiving buffer in byte.
    }
  deriving (Show, Eq)

instance Default Config where
  def =
    Config
      { address = AddressResolved $ NS.SockAddrInet 5432 $ NS.tupleToHostAddress (127, 0, 0, 1)
      , user = "postgres"
      , password = ""
      , database = ""
      , sendingBufferSize = 2 ^ (12 :: Int)
      , receptionBufferSize = 2 ^ (12 :: Int)
      }

-- | IP address.
data Address
  = AddressResolved NS.SockAddr -- ^ Address which is DNS resolved.
  | AddressNotResolved NS.HostName NS.ServiceName -- ^ Address which is not DNS resolved.
  deriving (Show, Eq)

-- | Set of server parameters.
type BackendParameters = Map BSS.ShortByteString BSS.ShortByteString

-- | PostgreSQL connection.
data Connection =
  Connection
    { socket          :: Socket
    , pid             :: Pid -- ^ The process ID of the server.
    , backendKey      :: BackendKey
    , parameters      :: BackendParameters -- ^ Set of server parameters.
    , sendingBuffer   :: Buffer
    , receptionBuffer :: Buffer
    , config          :: Config -- ^ Configuration of this connection.
    }

data Buffer = Buffer (ForeignPtr Word8) Int

type Salt = BS.ByteString

-- | Transaction state of a server.
data TransactionState
  = Idle -- ^ Not in a transaction block.
  | Block -- ^ In a transaction block.
  | Failed -- ^ Transaction failed.
  deriving (Show, Read, Eq, Enum)

-- | Proccess ID
type Pid = Int32

type BackendKey = Int32

type AttributeNumber = Int16

data TypeLength = VariableLength | FixedLength Int16 deriving (Show, Read, Eq, Ord)

type TypeModifier = Int32

-- | Format code of parameters of results.
data FormatCode = TextFormat | BinaryFormat deriving (Show, Read, Eq, Enum)

data BindParameterFormatCodes
  = BindParameterFormatCodesAllDefault
  | BindParameterFormatCodesAll FormatCode
  | BindParameterFormatCodesEach [FormatCode]
  deriving (Show, Read, Eq)

data BindResultFormatCodes
  = BindResultFormatCodesNothing
  | BindResultFormatCodesAllDefault
  | BindResultFormatCodesEach [FormatCode]
  deriving (Show, Read, Eq)

-- | Command tag, which means which SQL command is completed.
data CommandTag
  = InsertTag Oid Int
  | DeleteTag Int
  | UpdateTag Int
  | SelectTag Int
  | MoveTag Int
  | FetchTag Int
  | CopyTag Int -- since PostgreSQL 8.2
  | CreateTableTag
  | DropTableTag
  | BeginTag
  | CommitTag
  | RollbackTag
  | SetTag
  deriving (Show, Read, Eq)

data Response
  = AuthenticationResponse AuthenticationResponse
  | BackendKeyDataResponse BackendKeyData
  | CommandCompleteResponse CommandComplete
  | DataRowResponse DataRowRaw
  | ErrorResponse Error
  | NoticeResponse Notice
  | ParameterStatusResponse ParameterStatus
  | ReadyForQueryResponse ReadyForQuery
  | RowDescriptionResponse RowDescription
  | ParseCompleteResponse
  | BindCompleteResponse
  | EmptyQueryResponse
  | NoDataResponse
  | ParameterDescriptionResponse ParameterDescription
  | DebugResponse Debug -- XXX temporal implimentation

data AuthenticationResponse
  = AuthenticationOkResponse
  | AuthenticationCleartextPasswordResponse
  | AuthenticationMD5PasswordResponse AuthenticationMD5Password
  deriving (Show, Read, Eq)

newtype AuthenticationMD5Password = AuthenticationMD5Password Salt deriving (Show, Read, Eq)

data BackendKeyData = BackendKeyData Pid BackendKey deriving (Show, Read, Eq)

newtype CommandComplete = CommandComplete CommandTag deriving (Show, Read, Eq)

newtype DataRow r = DataRow r deriving (Show, Read, Eq)

newtype DataRowRaw = DataRowRaw [Raw] deriving (Show, Read, Eq)

newtype Error = Error ErrorFields deriving (Show, Read, Eq)

newtype Notice = Notice ErrorFields deriving (Show, Read, Eq)

data ParameterStatus = ParameterStatus BSS.ShortByteString BSS.ShortByteString deriving (Show, Read, Eq)

newtype ReadyForQuery = ReadyForQuery TransactionState deriving (Show, Read, Eq)

newtype RowDescription = RowDescription [ColumnInfo] deriving (Show, Read, Eq)

newtype ParameterDescription = ParameterDescription [Oid] deriving (Show, Read, Eq)

newtype Debug = Debug BS.ByteString deriving (Show, Read, Eq) -- XXX temporal implimentation

-- | Result of a “Execute” message.
data ExecuteResult
  = ExecuteComplete CommandTag -- ^ All records gotten.
  | ExecuteEmptyQuery -- ^ No records.
  | ExecuteSuspended -- ^ Records are left yet.
  deriving (Show, Read, Eq)

data DescribeResult
  = DescribePreparedStatementResult [Oid] [ColumnInfo]
  | DescribePortalResult [ColumnInfo]
  deriving (Show, Read, Eq)

-- https://www.postgresql.org/docs/current/protocol-error-fields.html
newtype ErrorFields = ErrorFields [(Char, BSS.ShortByteString)] deriving (Show, Read, Eq)

data TypeInfo
  = Basic Oid BS.ByteString
  deriving (Show, Read, Eq)

-- | Metadata of a column.
data ColumnInfo =
  ColumnInfo
    { name            :: BS.ByteString
    , tableOid        :: Oid
    , attributeNumber :: AttributeNumber
    , typeOid         :: Oid
    , typeLength      :: TypeLength
    , typeModifier    :: TypeModifier
    , formatCode      :: FormatCode
    }
    deriving (Show, Read, Eq)

type Carry = BS.ByteString

-- | Data without encoding nor decoding of a field.
newtype Raw = Raw (Maybe BS.ByteString) deriving (Eq, Ord)

instance Show Raw where
  show Null      = "NULL"
  show (Value a) = show (BS.unpack a)

instance Read Raw where
  readPrec =
    R.parens
      ( ( do
            R.lift $ R.expect $ R.Ident "NULL"
            pure Null
        )
        <|> (Value . BS.pack <$> readPrec)
      )

-- | @NULL@.
pattern Null :: Raw
pattern Null = Raw Nothing

-- | Not @NULL@.
pattern Value :: BS.ByteString -> Raw
pattern Value a = Raw (Just a)

{-# COMPLETE Null, Value #-}

-- | SQL query.
--
-- This 'Data.String.fromString' counts only ASCII, becouse it is the same with 'BS.ByteString'.
newtype Query = Query BS.ByteString deriving (Show, Read, Eq, Ord, IsString)

-- | To convert a type which means that it is not processed by the server to a respective type which means that it is processed by the server.
type family MessageResult m :: Type

-- | This represents a prepared statement which is already processed by a server.
data PreparedStatement =
  PreparedStatement
    { name          :: PreparedStatementName
    , parameterOids :: [Oid]
    , resultInfos   :: [ColumnInfo]
    }

instance Show PreparedStatement where
  show (PreparedStatement name parameterOids resultInfos) = "PreparedStatement " <> show name <> " " <> show parameterOids <> " " <> show resultInfos

instance Eq PreparedStatement where
  (PreparedStatement name0 parameterOids0 resultInfos0) == (PreparedStatement name1 parameterOids1 resultInfos1) = (name0, parameterOids0, resultInfos0) == (name1, parameterOids1, resultInfos1)

-- | This represents a prepared statement which is not yet processed by a server.
data PreparedStatementProcedure =
  PreparedStatementProcedure
    { name            :: PreparedStatementName
    , parameterLength :: Word
    , resultLength    :: Word
    , parameterOids   :: Maybe [Oid]
    , builder         :: BSB.Builder
    , parser          :: AP.Parser (MessageResult PreparedStatementProcedure)
    }

type instance MessageResult PreparedStatementProcedure = PreparedStatement

instance Show PreparedStatementProcedure where
  show (PreparedStatementProcedure name parameterLength resultLength oids _ _) =
    mconcat ["PreparedStatementProcedure ", show name, " ", show parameterLength, " ", show resultLength, " ", show oids, " _ _"]

-- | Name of a prepared statement.
newtype PreparedStatementName =
  PreparedStatementName BS.ByteString
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read, IsString)

-- | This represents a portal which is already processed by a server.
data Portal =
  Portal
    { name              :: PortalName
    , infos             :: [ColumnInfo]
    , preparedStatement :: PreparedStatement
    }

instance Show Portal where
  show (Portal name infos ps) = "Portal " <> show name <> " " <> show infos <> " (" <> show ps <> ")"

instance Eq Portal where
  (Portal name0 infos0 ps0) == (Portal name1 infos1 ps1) = (name0, infos0, ps0) == (name1, infos1, ps1)

-- | This represents a portal which is not yet processed by a server.
data PortalProcedure =
  PortalProcedure
    { name    :: PortalName
    , format  :: FormatCode
    , builder :: BSB.Builder
    , parser  :: AP.Parser (MessageResult PortalProcedure)
    }

type instance MessageResult PortalProcedure = (PreparedStatement, Portal)

instance Show PortalProcedure where
  show (PortalProcedure name format _ _) = "PortalProcedure " <> show name <> " " <> show format <> " _ _"

-- | Name of a portal.
newtype PortalName =
  PortalName BS.ByteString
  deriving stock (Eq, Ord)
  deriving newtype (Show, Read, IsString)

-- | This represents a result of a “Execute” message which is already processed by a server.
data Executed r =
  Executed
    { result  :: ExecuteResult
    , records :: [r]
    , portal  :: Portal
    }

instance Show r => Show (Executed r) where
  show (Executed r rs p) = "Executed " <> show r <> " " <> show rs <> " (" <> show p <> ")"

instance Eq r => Eq (Executed r) where
  (Executed r0 rs0 p0) == (Executed r1 rs1 p1) = (r0, rs0, p0) == (r1, rs1, p1)

-- | This represents a result of a “Execute” message which is not yet processed by a server.
data ExecutedProcedure r =
  ExecutedProcedure
    { builder :: BSB.Builder
    , parser  :: AP.Parser (MessageResult (ExecutedProcedure r))
    }

type instance MessageResult (ExecutedProcedure r) = (PreparedStatement, Portal, Executed r, Maybe ErrorFields)

instance Show (ExecutedProcedure r) where
  show (ExecutedProcedure _ _) = "ExecutedProcedure _ _"

-- | This represents a result of a “Close” message which is not yet processed by a server.
data CloseProcedure =
  CloseProcedure
    { builder :: BSB.Builder
    , parser  :: AP.Parser (MessageResult CloseProcedure)
    }

type instance MessageResult CloseProcedure = ()

instance Show CloseProcedure where
  show (CloseProcedure _ _) = "CloseProcedure _ _"

-- | Decoder of strings which may fail.
type StringDecoder = BS.ByteString -> Either String String

-- | Encoder of strings which may fail.
type StringEncoder = String -> Either String BS.ByteString

-- | This means that a field can be decoded as @a@.
class FromField a where
  -- | Decoder of a field.
  fromField :: MonadFail m => StringDecoder -> ColumnInfo -> Maybe BS.ByteString -> m a

-- | This means that a record can be parsed as @a@.
class FromRecord a where
  -- | Decoder of a record.
  fromRecord :: StringDecoder -> [ColumnInfo] -> AP.Parser a

-- | This means that @a@ can be encoded to a field.
class ToField a where
  -- | Encoder of a field.
  toField :: MonadFail m => BackendParameters -> StringEncoder -> Maybe Oid -> FormatCode -> a -> m (Maybe BS.ByteString)

-- | This means that @a@ can be encoded to a record.
class ToRecord a where
  -- | Encoder of a field.
  toRecord :: MonadFail m => BackendParameters -> StringEncoder -> Maybe [Oid] -> [FormatCode] -> a -> m [Maybe BS.ByteString]

-- | Type of PostgreSQL @sql_identifier@ type.
newtype SqlIdentifier = SqlIdentifier BS.ByteString deriving (Show, Read, Eq)

class Pretty a where
  pretty :: a -> String

instance Pretty Response where
  pretty (AuthenticationResponse r)       = pretty r
  pretty (CommandCompleteResponse r)      = pretty r
  pretty (DataRowResponse r)              = pretty r
  pretty (ErrorResponse r)                = pretty r
  pretty (NoticeResponse r)               = pretty r
  pretty (ParameterStatusResponse r)      = pretty r
  pretty (BackendKeyDataResponse r)       = pretty r
  pretty (ReadyForQueryResponse r)        = pretty r
  pretty (RowDescriptionResponse r)       = pretty r
  pretty ParseCompleteResponse            = "parse complete"
  pretty BindCompleteResponse             = "bind complete"
  pretty (ParameterDescriptionResponse r) = pretty r
  pretty EmptyQueryResponse               = "empty query"
  pretty NoDataResponse                   = "no data"
  pretty (DebugResponse r)                = pretty r

instance Pretty AuthenticationResponse where
  pretty AuthenticationOkResponse                = "authentication ok"
  pretty AuthenticationCleartextPasswordResponse = "authentication using cleartext"
  pretty (AuthenticationMD5PasswordResponse r)   = pretty r

instance Pretty AuthenticationMD5Password where
  pretty (AuthenticationMD5Password salt) = "authentication MD5 password:\n\tsalt: " <> simpleHex salt

instance Pretty CommandComplete where
  pretty (CommandComplete (InsertTag oid rows)) = "command complete:\n\ttag: insert \n\t\toid: " <> show oid <> "\n\t\trows: " <> show rows
  pretty (CommandComplete (DeleteTag rows)) = "command complete:\n\ttag: delete\n\t\trows: " <> show rows
  pretty (CommandComplete (UpdateTag rows)) = "command complete:\n\ttag: update\n\t\trows: " <> show rows
  pretty (CommandComplete (SelectTag rows)) = "command complete:\n\ttag: select\n\t\trows: " <> show rows
  pretty (CommandComplete (MoveTag rows)) = "command complete:\n\ttag: move\n\t\trows: " <> show rows
  pretty (CommandComplete (FetchTag rows)) = "command complete:\n\ttag: fetch\n\t\trows: " <> show rows
  pretty (CommandComplete (CopyTag rows)) = "command complete:\n\ttag: copy\n\t\trows: " <> show rows
  pretty (CommandComplete CreateTableTag) = "command complete:\n\ttag: create table"
  pretty (CommandComplete DropTableTag) = "command complete:\n\ttag: drop table"
  pretty (CommandComplete BeginTag) = "command complete:\n\ttag: begin"
  pretty (CommandComplete CommitTag) = "command complete:\n\ttag: commit"
  pretty (CommandComplete RollbackTag) = "command complete:\n\ttag: rollback"
  pretty (CommandComplete SetTag) = "command complete:\n\ttag: set"

instance Show r => Pretty (DataRow r) where
  pretty (DataRow record) = "data:\n" <> show record

instance Pretty DataRowRaw where
  pretty (DataRowRaw values) =
    "data:\n" <> intercalate "\n" (go <$> zip [0 :: Int ..] values)
    where
      go (idx, v) = "\t" <> show idx <> pretty v

instance Pretty Error where
  pretty (Error fields) = "error response:\n" <> indent (pretty fields)

instance Pretty Notice where
  pretty (Notice fields) = "notice response:\n" <> indent (pretty fields)

instance Pretty ErrorFields where
  pretty (ErrorFields errs) =
    let
      lookups = foldr go ("", "", "") :: [(Char, BSS.ShortByteString)] -> (BSS.ShortByteString, BSS.ShortByteString, BSS.ShortByteString)
      go ('S', largeS') (_, largeC', largeM') = (largeS', largeC', largeM')
      go ('C', largeC') (largeS', _, largeM') = (largeS', largeC', largeM')
      go ('M', largeM') (largeS', largeC', _) = (largeS', largeC', largeM')
      go _ a                                  = a
      (largeS, largeC, largeM) = lookups errs
      pp (code, message) = code : ": " <> shortByteStringToString message
    in
      shortByteStringToString (largeS <> " (" <> largeC <> "): " <> largeM) <> ('\n' : intercalate "\n" (pp <$> errs))

instance Pretty TransactionState where
  pretty Idle   = "idle"
  pretty Block  = "block"
  pretty Failed = "failed"

instance Pretty ParameterStatus where
  pretty (ParameterStatus key value) = "parameter:\n\t" <> shortByteStringToString key <> ": " <> shortByteStringToString value

instance Pretty BackendKeyData where
  pretty (BackendKeyData pid bk) = "cancellation key:\n\tpid: " <> show pid <> "\n\tbackend key: " <> show bk

instance Pretty ReadyForQuery where
  pretty (ReadyForQuery ts) = "ready for query:\n\ttransaction state: " <> (toLower <$> show ts)

instance Pretty RowDescription where
  -- This uses decoder of UTF-8 although this should read client_encoding parameter, because this is used for debugging.
  pretty (RowDescription infos) =
    "row description:\n" <> intercalate "\n" (go <$> infos)
    where
      go (ColumnInfo name tableOid attrNum typeOid len typeMod format) =
        "\t" <> BSU.toString name <> ":"
        <> "\n\t\ttable object ID: " <> show tableOid
        <> "\n\t\tcolumn attribute number: " <> show attrNum
        <> "\n\t\tdata type object ID: " <> show typeOid
        <> "\n\t\tdata type length: " <> pretty len
        <> "\n\t\ttype modifier: " <> show typeMod
        <> "\n\t\tformat: " <> pretty format

instance Pretty ParameterDescription where
  pretty (ParameterDescription oids) =
    "parameter description: " <> show oids

instance Pretty Debug where
  pretty (Debug bs) = "Debug:\n" <> prettyHex bs

instance Pretty TypeLength where
  pretty VariableLength  = "variable"
  pretty (FixedLength l) = show l

instance Pretty FormatCode where
  pretty TextFormat   = "text"
  pretty BinaryFormat = "binary"

instance Pretty Raw where
  pretty Null      = "NULL"
  pretty (Value r) = "Value [" <> simpleHex r <> "] " <> show (printableString r)

-- This uses decoder of UTF-8 although this should read client_encoding parameter, because this is used for debugging.
printableString :: BS.ByteString -> String
printableString bytes =
  let
    replacePrintable c
      | isPrint c = c
      | otherwise = '.'
  in
    replacePrintable <$> BSU.toString bytes

shortByteStringToString :: BSS.ShortByteString -> String
shortByteStringToString = ((chr . fromIntegral) <$>) . BSS.unpack

indent :: String -> String
indent = unlines . (('\t' :) <$>) . lines

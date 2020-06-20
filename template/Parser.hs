{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-missing-import-lists #-}-- for Prelude
{-# OPTIONS_GHC -Wno-orphans #-}

#include "MachDeps.h"

module Database.PostgreSQL.Pure.Internal.Parser
  ( response
  , authentication
  , authenticationOk
  , error
  , notice
  , parameterStatus
  , backendKeyData
  , readyForQuery
  , rowDescription
  , dataRow
  , dataRowRaw
  , commandComplete
  , parseComplete
  , bindComplete
  , portalSuspended
  , emptyQuery
  , closeComplete
  , noData
  , parameterDescription
  , skipUntilError
  , currentPos
  , column
  ) where

import           Database.PostgreSQL.Pure.Internal.Data          (AuthenticationMD5Password (AuthenticationMD5Password), AuthenticationResponse (AuthenticationMD5PasswordResponse, AuthenticationOkResponse, AuthenticationCleartextPasswordResponse),
                                                                  BackendKeyData (BackendKeyData),
                                                                  ColumnInfo (ColumnInfo, typeOid),
                                                                  CommandComplete (CommandComplete),
                                                                  CommandTag (BeginTag, CommitTag, CopyTag, CreateTableTag, DeleteTag, DropTableTag, FetchTag, InsertTag, MoveTag, RollbackTag, SelectTag, SetTag, UpdateTag),
                                                                  DataRow (DataRow), DataRowRaw (DataRowRaw),
                                                                  Debug (Debug), Error (Error),
                                                                  ErrorFields (ErrorFields),
                                                                  FormatCode (BinaryFormat, TextFormat),
                                                                  FromField (fromField), FromRecord (fromRecord),
                                                                  Notice (Notice), Oid (Oid),
                                                                  ParameterDescription (ParameterDescription),
                                                                  ParameterStatus (ParameterStatus), Raw (Null, Value),
                                                                  ReadyForQuery (ReadyForQuery), Response (..),
                                                                  RowDescription (RowDescription),
                                                                  SqlIdentifier (SqlIdentifier), StringDecoder,
                                                                  TransactionState (Block, Failed, Idle),
                                                                  TypeLength (FixedLength, VariableLength))
import qualified Database.PostgreSQL.Pure.Internal.Data          as Data
import qualified Database.PostgreSQL.Pure.Internal.MonadFail     as MonadFail
import qualified Database.PostgreSQL.Pure.Oid                    as Oid
import qualified Database.PostgreSQL.Simple.Time.Internal.Parser as Time

import           Prelude                                         hiding (error, fail)

import           Control.Exception                               (assert)
import           Control.Monad                                   (replicateM, unless, void)
import           Control.Monad.Fail                              (MonadFail (fail))
import qualified Data.Attoparsec.ByteString                      as AP
import qualified Data.Attoparsec.ByteString.Char8                as APC
import           Data.Attoparsec.Combinator                      ((<?>))
import qualified Data.Attoparsec.Combinator                      as AP
import qualified Data.Attoparsec.Internal.Types                  as API
import qualified Data.ByteString                                 as BS
import qualified Data.ByteString.Internal                        as BSI
import qualified Data.ByteString.Short                           as BSS
import qualified Data.ByteString.UTF8                            as BSU
import           Data.Functor                                    (($>))
import           Data.Int                                        (Int16, Int32, Int64, Int8)
import           Data.Kind                                       (Type)
import           Data.Maybe                                      (fromMaybe)
import           Data.Memory.Endian                              (BE, ByteSwap, fromBE)
import           Data.Scientific                                 (Scientific, scientific)
import qualified Data.Text                                       as Text
import           Data.Time                                       (Day, DiffTime, LocalTime, TimeOfDay, TimeZone,
                                                                  UTCTime, utc)
import           Data.Tuple.Single                               (Single, pattern Single)
import           Data.Word                                       (Word16, Word32, Word64, Word8)
import           Foreign                                         (withForeignPtr)
import           Foreign.Storable                                (Storable, peekByteOff, sizeOf)
import           GHC.Stack                                       (HasCallStack, callStack, prettyCallStack)
import qualified PostgreSQL.Binary.Decoding                      as BD
import           System.IO.Unsafe                                (unsafeDupablePerformIO)

#if MIN_VERSION_base(4,13,0)
import           Control.Applicative                             ((<|>))
#else
import           Control.Applicative                             ((*>), (<|>))
#endif

response :: AP.Parser Response
response =
  AuthenticationResponse <$> authentication
  <|> ErrorResponse <$> error
  <|> NoticeResponse <$> notice
  <|> ParameterStatusResponse <$> parameterStatus
  <|> BackendKeyDataResponse <$> backendKeyData
  <|> ReadyForQueryResponse <$> readyForQuery
  <|> RowDescriptionResponse <$> rowDescription
  <|> DataRowResponse <$> dataRowRaw
  <|> CommandCompleteResponse <$> commandComplete
  <|> (parseComplete >> pure ParseCompleteResponse)
  <|> (bindComplete >> pure BindCompleteResponse)
  <|> (emptyQuery >> pure EmptyQueryResponse)
  <|> DebugResponse <$> debug

responseHeader :: AP.Parser Char -> AP.Parser (Char, Int)
responseHeader identParser =
  (<?> "response header") $ do
    ident <- identParser
    len32 <- anyInt32BE
    let len = fromIntegral len32 - 4 :: Int -- minus length of "length field"
    pure (ident, len)

authentication :: AP.Parser AuthenticationResponse
authentication =
  (<?> "authentication") $ do
    (_, len) <- responseHeader $ APC.char 'R'
    checkConsumed len $ do
      method <- anyInt32BE
      case method of
        0 -> pure AuthenticationOkResponse
        3 -> pure AuthenticationCleartextPasswordResponse
        5 -> AuthenticationMD5PasswordResponse . AuthenticationMD5Password . BS.copy <$> AP.take 4
        t -> fail $ "not yet implemeted authentication type: " <> show t

authenticationOk :: AP.Parser ()
authenticationOk =
  (<?> "authentication ok") $ do
    (_, len) <- responseHeader $ APC.char 'R'
    checkConsumed len $ void $ int32BE 0

error :: AP.Parser Error
error =
  (<?> "error") $ do
    (_, len) <- responseHeader $ APC.char 'E'
    checkConsumed len $ Error . ErrorFields <$> list ((,) <$> APC.anyChar <*> (BSS.toShort <$> string))

notice :: AP.Parser Notice
notice =
  (<?> "notice") $ do
    (_, len) <- responseHeader $ APC.char 'N'
    checkConsumed len $ Notice . ErrorFields <$> list ((,) <$> APC.anyChar <*> (BSS.toShort <$> string))

parameterStatus :: AP.Parser ParameterStatus
parameterStatus =
  (<?> "parameter status") $ do
    (_, len) <- responseHeader $ APC.char 'S'
    checkConsumed len $ ParameterStatus <$> (BSS.toShort <$> string) <*> (BSS.toShort <$> string)

backendKeyData :: AP.Parser BackendKeyData
backendKeyData =
  (<?> "backend key data") $ do
    (_, len) <- responseHeader $ APC.char 'K'
    checkConsumed len $ BackendKeyData <$> anyInt32BE <*> anyInt32BE

readyForQuery :: AP.Parser ReadyForQuery
readyForQuery =
  (<?> "ready for query") $ do
    (_, len) <- responseHeader $ APC.char 'Z'
    checkConsumed len $ do
      t <- APC.anyChar
      case t of
        'I' -> pure $ ReadyForQuery Idle
        'T' -> pure $ ReadyForQuery Block
        'E' -> pure $ ReadyForQuery Failed
        _   -> fail "invalid transacion state character"

rowDescription :: AP.Parser RowDescription
rowDescription =
  (<?> "row description") $ do
    (_, len) <- responseHeader $ APC.char 'T'
    checkConsumed len $ do
      fieldCount <- anyInt16BE
      RowDescription <$>
        replicateM
          (fromIntegral fieldCount)
          (ColumnInfo <$> string <*> oid <*> anyInt16BE <*> oid <*> typeLength <*> anyInt32BE <*> formatCode)

dataRow :: FromRecord r => StringDecoder -> [ColumnInfo] -> AP.Parser (DataRow r)
dataRow decode infos =
  (<?> "data row") $ do
    (_, len) <- responseHeader $ APC.char 'D'
    checkConsumed len $ do
      void $ int16BE (fromIntegral $ length infos)
      DataRow <$> fromRecord decode infos

dataRowRaw :: AP.Parser DataRowRaw
dataRowRaw =
  (<?> "data row raw") $ do
    (_, len) <- responseHeader $ APC.char 'D'
    checkConsumed len $ do
      columnCount <- anyInt16BE
      let
        go = do
          l <- anyInt32BE
          case l of
            (-1) -> pure Null
            _    -> Value . BS.copy <$> AP.take (fromIntegral l)
      DataRowRaw <$>
        replicateM
          (fromIntegral columnCount)
          go

commandComplete :: AP.Parser CommandComplete
commandComplete =
  (<?> "command complete") $ do
    (_, len) <- responseHeader $ APC.char 'C'
    checkConsumed len $
      do
        void $ APC.string "INSERT "
        o <- Oid <$> APC.decimal
        void $ APC.char ' '
        r <- APC.decimal
        void $ AP.word8 0
        pure $ CommandComplete $ InsertTag o r
      <|>
      do
        void $ APC.string "DELETE "
        r <- APC.decimal
        void $ AP.word8 0
        pure $ CommandComplete $ DeleteTag r
      <|>
      do
        void $ APC.string "UPDATE "
        r <- APC.decimal
        void $ AP.word8 0
        pure $ CommandComplete $ UpdateTag r
      <|>
      do
        void $ APC.string "SELECT "
        r <- APC.decimal
        void $ AP.word8 0
        pure $ CommandComplete $ SelectTag r
      <|>
      do
        void $ APC.string "MOVE "
        r <- APC.decimal
        void $ AP.word8 0
        pure $ CommandComplete $ MoveTag r
      <|>
      do
        void $ APC.string "FETCH "
        r <- APC.decimal
        void $ AP.word8 0
        pure $ CommandComplete $ FetchTag r
      <|>
      do
        void $ APC.string "COPY "
        r <- APC.decimal
        void $ AP.word8 0
        pure $ CommandComplete $ CopyTag r
      <|> APC.string "CREATE TABLE" *> AP.word8 0 $> CommandComplete CreateTableTag
      <|> APC.string "DROP TABLE" *> AP.word8 0 $> CommandComplete DropTableTag
      <|> APC.string "BEGIN" *> AP.word8 0 $> CommandComplete BeginTag
      <|> APC.string "COMMIT" *> AP.word8 0 $> CommandComplete CommitTag
      <|> APC.string "ROLLBACK" *> AP.word8 0 $> CommandComplete RollbackTag
      <|> APC.string "SET" *> AP.word8 0 $> CommandComplete SetTag

parseComplete :: AP.Parser ()
parseComplete =
  (<?> "parse complete") $ do
    void $ APC.char8 '1'
    void $ int32BE 4

bindComplete :: AP.Parser ()
bindComplete =
  (<?> "bind complete") $ do
    void $ APC.char8 '2'
    void $ int32BE 4

noData :: AP.Parser ()
noData =
  (<?> "no data") $ do
    void $ APC.char8 'n'
    void $ int32BE 4

parameterDescription :: AP.Parser ParameterDescription
parameterDescription =
  (<?> "parameter description") $ do
    (_, len) <- responseHeader $ APC.char 't'
    checkConsumed len $ do
      n <- anyInt16BE
      ParameterDescription <$> replicateM (fromIntegral n) oid

emptyQuery :: AP.Parser ()
emptyQuery =
  (<?> "empty query") $ do
    void $ APC.char8 'I'
    void $ int32BE 4

portalSuspended :: AP.Parser ()
portalSuspended =
  (<?> "portal suspended") $ do
    void $ APC.char8 's'
    void $ int32BE 4

closeComplete :: AP.Parser ()
closeComplete =
  (<?> "close complete") $ do
    void $ APC.char8 '3'
    void $ int32BE 4

skipUntilError :: AP.Parser Error
skipUntilError =
  (<?> "skip until error") $ do
    (ident, len) <- responseHeader APC.anyChar
    case ident of
      'E' -> checkConsumed len $ Error . ErrorFields <$> list ((,) <$> APC.anyChar <*> (BSS.toShort <$> string))
      _   -> AP.take len >> skipUntilError

debug :: AP.Parser Debug
debug = do
  ident <- AP.anyWord8
  len <- AP.lookAhead anyInt32BE
  bs <- AP.take $ fromIntegral len
  pure $ Debug $ BS.cons ident bs

satisfyN :: Int -> (BS.ByteString -> a) -> (a -> Bool) -> AP.Parser a
satisfyN l f p = do
  bs <- AP.take l
  let a = f bs
  if p a
    then pure a
    else fail "satisfy n"

satisfyStorable :: forall a. Storable a => (BS.ByteString -> a) -> (a -> Bool) -> AP.Parser a
satisfyStorable f p = satisfyN (sizeOf (undefined :: a)) f p <?> "satisfy storable"

type family Unsigned a :: Type
type instance Unsigned Word8 = Word8
type instance Unsigned Word16 = Word16
type instance Unsigned Word32 = Word32
type instance Unsigned Word64 = Word64
type instance Unsigned Int8 = Word8
type instance Unsigned Int16 = Word16
type instance Unsigned Int32 = Word32
type instance Unsigned Int64 = Word64

satisfyIntegralBE :: forall a. (Integral a, Storable a, Integral (Unsigned a), ByteSwap (Unsigned a)) => (a -> Bool) -> AP.Parser a
satisfyIntegralBE p = satisfyStorable (fromIntegral . castByteSwapBE @(Unsigned a)) p <?> "satisfy integral big endian"

anyIntegralBE :: (Integral a, Storable a, Integral (Unsigned a), ByteSwap (Unsigned a)) => AP.Parser a
anyIntegralBE = satisfyIntegralBE (const True) <?> "any integral big endian"

anyInt16BE :: AP.Parser Int16
anyInt16BE = anyIntegralBE <?> "any int16 big endian"

anyInt32BE :: AP.Parser Int32
anyInt32BE = anyIntegralBE <?> "any int32 big endian"

integralBE :: (Integral a, Storable a, Integral (Unsigned a), ByteSwap (Unsigned a)) => a -> AP.Parser a
integralBE n = satisfyIntegralBE (== n) <?> "integral big endian"

int16BE :: Int16 -> AP.Parser Int16
int16BE n = integralBE n <?> "int16 big endian"

int32BE :: Int32 -> AP.Parser Int32
int32BE n = integralBE n <?> "int32 big endian"

castByteSwapBE :: forall a. ByteSwap a => BS.ByteString -> a
castByteSwapBE (BSI.PS fptr off len) =
  assert (sizeOf (undefined :: a) == len) $
    let
      be :: BE a
      be =
        unsafeDupablePerformIO $
          withForeignPtr fptr $ \ptr ->
            peekByteOff ptr off
    in fromBE be

string :: AP.Parser BS.ByteString
string = AP.takeWhile (/= 0) <* AP.word8 0 <?> "string"

list :: AP.Parser a -> AP.Parser [a]
list p = (AP.word8 0 >> pure []) <|> (:) <$> p <*> list p <?> "list"

typeLength :: AP.Parser TypeLength
typeLength =
  (<?> "type length") $ do
    len <- anyInt16BE
    if len < 0
      then pure VariableLength
      else pure $ FixedLength len

formatCode :: AP.Parser FormatCode
formatCode =
  (<?> "format code") $ do
    code <- anyInt16BE
    case code of
      0 -> pure TextFormat
      1 -> pure BinaryFormat
      _ -> fail "invalid format code"

oid :: AP.Parser Oid
oid = Oid <$> anyInt32BE <?> "OID"

checkConsumed :: HasCallStack => Int -> AP.Parser a -> AP.Parser a
checkConsumed expected parser = do
  API.Pos startPos <- currentPos
  r <- parser
  API.Pos endPos <- currentPos
  let consumed = endPos - startPos
  unless (expected == consumed) $
    fail $ "length mismatch: expected: " <> show expected <> ", consumed: " <> show consumed <> "\n" <> prettyCallStack callStack
  pure r

currentPos :: AP.Parser API.Pos
currentPos = API.Parser $ \t pos more _lose suc -> suc t pos more pos

instance FromField Bool where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
    | typeOid == Oid.bool
    = case formatCode of
        TextFormat | v == "t" -> pure True
                   | v == "f" -> pure False
                   | otherwise -> fail (show (BSU.toString v) <> " is not expected as bool")
        BinaryFormat -> valueParser BD.bool v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: Bool"

instance FromField Int where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
#if WORD_SIZE_IN_BITS < 32 /* the width of Int is wider than 30 bits */
    | typeOid == Oid.int2
#elif WORD_SIZE_IN_BITS < 64
    | typeOid `elem` [Oid.int2, Oid.int4]
#else
    | typeOid `elem` [Oid.int2, Oid.int4, Oid.int8]
#endif
    = case formatCode of
        TextFormat   -> attoparsecParser (APC.signed APC.decimal) v
        BinaryFormat -> valueParser BD.int v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: Int"

instance FromField Int16 where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
    | typeOid == Oid.int2
    = case formatCode of
        TextFormat   -> attoparsecParser (APC.signed APC.decimal) v
        BinaryFormat -> valueParser BD.int v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: Int16"

instance FromField Int32 where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
    | typeOid `elem` [Oid.int2, Oid.int4]
    = case formatCode of
        TextFormat   -> attoparsecParser (APC.signed APC.decimal) v
        BinaryFormat -> valueParser BD.int v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: Int32"

instance FromField Int64 where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
    | typeOid `elem` [Oid.int2, Oid.int4, Oid.int8]
    = case formatCode of
        TextFormat   -> attoparsecParser (APC.signed APC.decimal) v
        BinaryFormat -> valueParser BD.int v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: Int64"

instance FromField Scientific where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
    | typeOid `elem` [Oid.int2, Oid.int4, Oid.int8]
    = (flip scientific 0 <$>) $
        case formatCode of
          TextFormat   -> attoparsecParser (APC.signed APC.decimal) v
          BinaryFormat -> valueParser BD.int v
    | typeOid == Oid.numeric
    = case formatCode of
        TextFormat   -> attoparsecParser APC.scientific v
        BinaryFormat -> valueParser BD.numeric v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: Scientific"

instance FromField Float where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
    | typeOid == Oid.float4
    = case formatCode of
        TextFormat   -> attoparsecParser APC.rational v
        BinaryFormat -> valueParser BD.float4 v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: Float"

instance FromField Double where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
    | typeOid == Oid.float4
    = case formatCode of
        TextFormat   -> attoparsecParser APC.rational v
        BinaryFormat -> realToFrac <$> valueParser BD.float4 v
    | typeOid == Oid.float8
    = case formatCode of
        TextFormat   -> attoparsecParser APC.double v
        BinaryFormat -> valueParser BD.float8 v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: Double"

instance FromField Oid where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
    | typeOid == Oid.oid
    = (Oid <$>) $
        case formatCode of
          TextFormat   -> attoparsecParser APC.decimal v
          BinaryFormat -> valueParser BD.int v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: Oid"

instance FromField Char where
  fromField decode ColumnInfo { typeOid } (Just v)
    | typeOid == Oid.char
    = do
        str <- MonadFail.fromEither $ decode v
        case (str :: String) of
          [c] -> pure c
          _   -> fail $ "expected 1 character, actual " <> show (length str) <> " characters"
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: Char"

instance FromField String where
  fromField decode ColumnInfo { typeOid } (Just v)
    | typeOid `elem` [Oid.text, Oid.bpchar, Oid.varchar, Oid.name]
    = MonadFail.fromEither $ decode v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: String"

instance FromField BS.ByteString where
  fromField _ ColumnInfo { typeOid } (Just v)
    | typeOid `elem` [Oid.text, Oid.bpchar, Oid.varchar, Oid.name, Oid.bytea] = pure v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: ByteString (strict)"

instance FromField Day where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
    | typeOid == Oid.date
    = case formatCode of
        TextFormat   -> attoparsecParser Time.day v
        BinaryFormat -> valueParser BD.date v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: Day"

instance FromField TimeOfDay where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
    | typeOid == Oid.time
    = case formatCode of
        TextFormat   -> attoparsecParser Time.timeOfDay v
        BinaryFormat -> valueParser BD.time_int v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: TimeOfDay"

instance FromField (TimeOfDay, TimeZone) where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
    | typeOid == Oid.timetz
    = case formatCode of
        TextFormat   -> attoparsecParser ((,) <$> Time.timeOfDay <*> (fromMaybe utc <$> Time.timeZone)) v
        BinaryFormat -> valueParser BD.timetz_int v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: (TimeOfDay, TimeZone)"

instance FromField LocalTime where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
    | typeOid == Oid.timestamp
    = case formatCode of
        TextFormat   -> attoparsecParser Time.localTime v
        BinaryFormat -> valueParser BD.timestamp_int v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: LocalTime"

instance FromField UTCTime where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
    | typeOid == Oid.timestamptz
    = case formatCode of
        TextFormat   -> attoparsecParser Time.utcTime v
        BinaryFormat -> valueParser BD.timestamptz_int v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: UTCTime"

instance FromField DiffTime where
  fromField _ ColumnInfo { typeOid, Data.formatCode } (Just v)
    | typeOid == Oid.interval
    = case formatCode of
        TextFormat   -> attoparsecParser Time.diffTime v
        BinaryFormat -> valueParser BD.interval_int v
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: DiffTime"

instance FromField SqlIdentifier where
  fromField decode info@ColumnInfo { typeOid } v@(Just _)
    | typeOid == Oid.sqlIdentifier = SqlIdentifier <$> fromField decode info { typeOid = Oid.varchar } v -- pg_type.typbasetype
  fromField _ ColumnInfo { typeOid } _ = fail $ "type mismatch (FromField): OID: " <> show typeOid <> ", Haskell: SqlIdentifier"

instance FromField Raw where
  fromField _ _ (Just v) = pure $ Value v
  fromField _ _ Nothing  = pure Null

instance FromField a => FromField (Maybe a) where
  fromField decode i v@(Just _) = Just <$> fromField decode i v
  fromField _ _ Nothing         = pure Nothing

-- 0 tuple
instance FromRecord () where
  fromRecord _ [] = pure ()
  fromRecord _ is = fail $ "length mismatch: expected 0: actual: " <> show (length is)

-- 1 tuple
instance {-# OVERLAPPABLE #-} (FromField a, Single c, t ~ c a) => FromRecord t where
  fromRecord decode [i] = Single <$> column decode i
  fromRecord _ is       = fail $ "length mismatch: expected 1: actual: " <> show (length is)

---- embed 2

---- embed 3

---- embed 4

---- embed 5

---- embed 6

---- embed 7

---- embed 8

---- embed 9

---- embed 10

---- embed 11

---- embed 12

---- embed 13

---- embed 14

---- embed 15

---- embed 16

---- embed 17

---- embed 18

---- embed 19

---- embed 20

---- embed 21

---- embed 22

---- embed 23

---- embed 24

---- embed 25

---- embed 26

---- embed 27

---- embed 28

---- embed 29

---- embed 30

---- embed 31

---- embed 32

---- embed 33

---- embed 34

---- embed 35

---- embed 36

---- embed 37

---- embed 38

---- embed 39

---- embed 40

---- embed 41

---- embed 42

---- embed 43

---- embed 44

---- embed 45

---- embed 46

---- embed 47

---- embed 48

---- embed 49

---- embed 50

---- embed 51

---- embed 52

---- embed 53

---- embed 54

---- embed 55

---- embed 56

---- embed 57

---- embed 58

---- embed 59

---- embed 60

---- embed 61

---- embed 62

-- list
instance FromField a => FromRecord [a] where
  fromRecord decode is = sequence $ column decode <$> is

-- | For implementing 'fromRecord'.
column :: FromField a => StringDecoder -> ColumnInfo -> AP.Parser a
column decode info = do
  l <- anyInt32BE
  case l of
    (-1) -> fromField decode info Nothing
    _    -> fromField decode info . Just =<< AP.take (fromIntegral l)

attoparsecParser :: MonadFail m => AP.Parser a -> BS.ByteString -> m a
attoparsecParser parser string =
  case AP.parseOnly (parser <* AP.endOfInput) string of
    Right a -> pure a
    Left e  -> fail e

valueParser :: MonadFail m => BD.Value a -> BS.ByteString -> m a
valueParser parser string =
  case BD.valueParser parser string of
    Right a -> pure a
    Left e  -> fail $ Text.unpack e

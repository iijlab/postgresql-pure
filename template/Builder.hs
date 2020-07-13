{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

#include "MachDeps.h"

module Database.PostgreSQL.Pure.Internal.Builder
  ( startup
  , password
  , terminate
  , query
  , parse
  , bind
  , execute
  , describePreparedStatement
  , describePortal
  , flush
  , sync
  , closePreparedStatement
  , closePortal
  ) where

import           Database.PostgreSQL.Pure.Internal.Data           (BindParameterFormatCodes (BindParameterFormatCodesAll, BindParameterFormatCodesAllDefault, BindParameterFormatCodesEach),
                                                                   BindResultFormatCodes (BindResultFormatCodesAllDefault, BindResultFormatCodesEach, BindResultFormatCodesNothing),
                                                                   FormatCode (BinaryFormat, TextFormat), Oid (Oid),
                                                                   PortalName (PortalName),
                                                                   PreparedStatementName (PreparedStatementName),
                                                                   Query (Query), TimeOfDayWithTimeZone (TimeOfDayWithTimeZone), ToField (toField),
                                                                   ToRecord (toRecord))
import           Database.PostgreSQL.Pure.Internal.Exception      (cantReachHere)
import qualified Database.PostgreSQL.Pure.Internal.MonadFail      as MonadFail
import qualified Database.PostgreSQL.Pure.Oid                     as Oid
import qualified Database.PostgreSQL.Simple.Time.Internal.Printer as Time

import           Control.Exception.Safe                           (assert)
import qualified Data.Bool                                        as B
import qualified Data.ByteString                                  as BS
import qualified Data.ByteString.Builder                          as BSB
import qualified Data.ByteString.Builder.Prim                     as BSBP
import qualified Data.ByteString.Lazy                             as BSL
import qualified Data.Double.Conversion.ByteString                as DC
import           Data.Fixed                                       (Fixed (MkFixed), HasResolution, Pico, resolution)
import           Data.Int                                         (Int16, Int32, Int64)
import qualified Data.Map.Strict                                  as M
import           Data.Scientific                                  (FPFormat (Exponent), Scientific, formatScientific,
                                                                   scientific)
import           Data.Time                                        (Day, DiffTime, NominalDiffTime, TimeOfDay,
                                                                   UTCTime)
import           Data.Time.LocalTime                              (LocalTime)
import           Data.Tuple.Single                                (Single, pattern Single)
import qualified PostgreSQL.Binary.Encoding                       as BE

startup
  :: String -- ^ user name. ASCII text.
  -> String -- ^ database name. ASCII text.
  -> BSB.Builder
startup user database =
  let
    len =
      4 -- length field
      + 2 -- protocol major version
      + 2 -- protocol minor version
      + 5 -- "user\0"
      + length user
      + 1 -- '\0'
      + 9 -- "database\0"
      + length database
      + 1 -- '\0'
      + 1 -- '\0'
  in
    BSB.int32BE (fromIntegral len)
    <> BSB.int16BE 3 -- protocol major version
    <> BSB.int16BE 0 -- protocol minor version
    <> BSB.string7 "user\0"
    <> BSB.string7 user
    <> BSB.char7 '\0'
    <> BSB.string7 "database\0" <> BSB.string7 database <> BSB.char7 '\0'
    <> BSB.char7 '\0'

password
  :: BS.ByteString -- ^ password, which may be hashed. ASCII text.
  -> BSB.Builder
password password =
  let
    len = 4 + BS.length password + 1
  in
    BSB.char7 'p'
    <> BSB.int32BE (fromIntegral len)
    <> BSB.byteString password
    <> BSB.char7 '\0'

query :: Query -> BSB.Builder
query (Query q) =
  let
    len = 4 + BS.length q + 1
  in
    BSB.char7 'Q'
    <> BSB.int32BE (fromIntegral len)
    <> BSB.byteString q
    <> BSB.char7 '\0'

terminate :: BS.ByteString
terminate = BS.pack [0x58, 0, 0, 0, 4]

parse
  :: PreparedStatementName
  -> Query
  -> [Oid] -- ^ OIDs of data types of parameters
  -> BSB.Builder
parse (PreparedStatementName name) (Query q) oids =
  let
    len = 4 + BS.length name + 1 + BS.length q + 1 + 2 + noids * 4
    noids = length oids
  in
    BSB.char7 'P'
    <> BSB.int32BE (fromIntegral len)
    <> BSB.byteString name
    <> BSB.char7 '\0'
    <> BSB.byteString q
    <> BSB.char7 '\0'
    <> BSB.int16BE (fromIntegral noids)
    <> mconcat (BSB.int32BE . (\(Oid n) -> n) <$> oids)

bind
  :: PortalName
  -> PreparedStatementName
  -> BindParameterFormatCodes
  -> [Maybe BS.ByteString]
  -> BindResultFormatCodes
  -> BSB.Builder
bind (PortalName portalName) (PreparedStatementName preparedStatementName) parameterFormatCodes parameters resultFormatCodes =
  let
    len =
      4 -- length field
      + BS.length portalName
      + 1 -- '\0'
      + BS.length preparedStatementName
      + 1 -- '\0'
      + 2 -- the number of parameter format codes
      + ( case parameterFormatCodes of -- format codes
            BindParameterFormatCodesAllDefault -> 0
            BindParameterFormatCodesAll _      -> 2
            BindParameterFormatCodesEach cs    -> 2 * length cs
        )
      + 2 -- the number of parameters
      + 4 * length parameters -- length field of each parameters
      + sum ((\p -> case p of Just bs -> BS.length bs; Nothing -> 0) <$> parameters) -- parameters themselves
      + 2 -- the number of result format codes
      + ( case resultFormatCodes of -- format codes
            BindResultFormatCodesNothing    -> 0
            BindResultFormatCodesAllDefault -> 0
            BindResultFormatCodesEach cs    -> 2 * length cs
        )
  in
    BSB.char7 'B'
    <> BSB.int32BE (fromIntegral len)
    <> BSB.byteString portalName
    <> BSB.char7 '\0'
    <> BSB.byteString preparedStatementName
    <> BSB.char7 '\0'
    <> ( case parameterFormatCodes of
           BindParameterFormatCodesAllDefault -> BSB.int16BE 0
           BindParameterFormatCodesAll c -> BSB.int16BE 1 <> BSB.int16BE (fromIntegral $ fromEnum c)
           BindParameterFormatCodesEach cs -> BSB.int16BE (fromIntegral $ length cs) <> mconcat (BSB.int16BE . fromIntegral . fromEnum <$> cs)
       )
    <> BSB.int16BE (fromIntegral $ length parameters)
    <> mconcat
         ( ( \p ->
               case p of
                 Just bs -> BSB.int32BE (fromIntegral $ BS.length bs) <> BSB.byteString bs
                 Nothing -> BSB.int32BE (-1)
           ) <$> parameters
         )
    <> ( case resultFormatCodes of
           BindResultFormatCodesNothing -> BSB.int16BE 0
           BindResultFormatCodesAllDefault -> BSB.int16BE 1
           BindResultFormatCodesEach cs -> BSB.int16BE (fromIntegral $ length cs) <> mconcat (BSB.int16BE . fromIntegral . fromEnum <$> cs)
       )

execute
  :: PortalName
  -> Int -- ^ limit of the number of rows
  -> BSB.Builder
execute (PortalName name) limitRows =
  let
    len = 4 + BS.length name + 1 + 4
  in
    BSB.char7 'E'
    <> BSB.int32BE (fromIntegral len)
    <> BSB.byteString name
    <> BSB.char7 '\0'
    <> BSB.int32BE (fromIntegral limitRows)

flush :: BS.ByteString
flush = BS.pack [0x48, 0, 0, 0, 4]

sync :: BS.ByteString
sync = BS.pack [0x53, 0, 0, 0, 4]

describePreparedStatement :: PreparedStatementName -> BSB.Builder
describePreparedStatement (PreparedStatementName name) = doDescribe 'S' name

closePreparedStatement :: PreparedStatementName -> BSB.Builder
closePreparedStatement (PreparedStatementName name) = doClose 'S' name

describePortal :: PortalName -> BSB.Builder
describePortal (PortalName name) = doDescribe 'P' name

closePortal :: PortalName -> BSB.Builder
closePortal (PortalName name) = doClose 'P' name

doDescribe :: Char -> BS.ByteString -> BSB.Builder
doDescribe typ name =
  let
    len = 4 + 1 + BS.length name + 1
  in
    BSB.char7 'D'
    <> BSB.int32BE (fromIntegral len)
    <> BSB.char7 typ
    <> BSB.byteString name
    <> BSB.char7 '\0'

doClose :: Char -> BS.ByteString -> BSB.Builder
doClose typ name =
  let
    len = 4 + 1 + BS.length name + 1
  in
    BSB.char7 'C'
    <> BSB.int32BE (fromIntegral len)
    <> BSB.char7 typ
    <> BSB.byteString name
    <> BSB.char7 '\0'

instance ToField () where
  toField _ _ _ _ _ = fail "no values for units"

instance ToField Bool where
  toField _ _ Nothing TextFormat = pure . Just . B.bool "TRUE" "FALSE"
  toField _ _ Nothing BinaryFormat = pure . Just . BE.encodingBytes . BE.bool
  toField backendParams encode (Just o) f | o == Oid.bool = toField backendParams encode Nothing f
                                          | otherwise = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: Bool"

instance ToField Int where
#if WORD_SIZE_IN_BITS > 64
  toField _ _ _ _ _ = fail "the Int's size is too large, larger then 64 bits"
#else
  toField _ _ Nothing TextFormat = pure . Just. BSL.toStrict . BSB.toLazyByteString . BSB.intDec
#if WORD_SIZE_IN_BITS > 32
  toField _ _ Nothing BinaryFormat = pure . Just . BE.encodingBytes . BE.int8_int64 . fromIntegral
  toField backendParams encode (Just o) TextFormat | o == Oid.int8 = toField backendParams encode Nothing TextFormat
  toField backendParams encode (Just o) BinaryFormat | o == Oid.int8 = toField backendParams encode Nothing BinaryFormat
#else /* the width of Int is wider than 30 bits */
  toField _ _ Nothing BinaryFormat = pure . Just . BE.encodingBytes . BE.int4_int32 . fromIntegral
  toField backendParams encode (Just o) TextFormat | o `elem` [Oid.int4, Oid.int8] = toField backendParams encode Nothing TextFormat
  toField backendParams encode (Just o) BinaryFormat | o == Oid.int4 = toField backendParams encode Nothing BinaryFormat
                                                     | o == Oid.int8 = pure . Just . BE.encodingBytes . BE.int8_int64 . fromIntegral
#endif
  toField _ _ (Just o) _ = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: Int"
#endif

instance ToField Int16 where
  toField _ _ Nothing TextFormat = pure . Just . BSL.toStrict . BSB.toLazyByteString . BSB.int16Dec
  toField _ _ Nothing BinaryFormat = pure . Just . BE.encodingBytes . BE.int2_int16
  toField backendParams encode (Just o) f | o == Oid.int2 = toField backendParams encode Nothing f
                                          | otherwise = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: Int16"

instance ToField Int32 where
  toField _ _ Nothing TextFormat = pure . Just . BSL.toStrict . BSB.toLazyByteString . BSB.int32Dec
  toField _ _ Nothing BinaryFormat = pure . Just . BE.encodingBytes . BE.int4_int32
  toField backendParams encode (Just o) TextFormat | o `elem` [Oid.int4, Oid.int8] = toField backendParams encode Nothing TextFormat
  toField backendParams encode (Just o) BinaryFormat | o == Oid.int4 = toField backendParams encode Nothing BinaryFormat
                                                     | o == Oid.int8 = pure . Just . BE.encodingBytes . BE.int8_int64 . fromIntegral
  toField _ _ (Just o) _ = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: Int32"

instance ToField Int64 where
  toField _ _ Nothing TextFormat = pure . Just . BSL.toStrict . BSB.toLazyByteString . BSB.int64Dec
  toField _ _ Nothing BinaryFormat = pure . Just . BE.encodingBytes . BE.int8_int64
  toField backendParams encode (Just o) f | o == Oid.int8 = toField backendParams encode Nothing f
                                          | otherwise = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: Int64"

instance ToField Float where
  toField _ _ Nothing TextFormat = pure . Just . DC.toShortest . realToFrac
  toField _ _ Nothing BinaryFormat = pure . Just . BE.encodingBytes . BE.float4
  toField backendParams encode (Just o) TextFormat | o `elem` [Oid.float4, Oid.float8] = toField backendParams encode Nothing TextFormat
  toField backendParams encode (Just o) BinaryFormat | o == Oid.float4 = toField backendParams encode Nothing BinaryFormat
                                                     | o == Oid.float8 = pure . Just . BE.encodingBytes . BE.float8 . realToFrac
  toField _ _ (Just o) _ = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: Float"

instance ToField Double where
  toField _ _ Nothing TextFormat = pure . Just . DC.toShortest
  toField _ _ Nothing BinaryFormat = pure . Just . BE.encodingBytes . BE.float8
  toField backendParams encode (Just o) f | o == Oid.float8 = toField backendParams encode Nothing f
                                          | otherwise = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: Double"

instance ToField Scientific where
  toField _ encode Nothing TextFormat = (Just <$>) . MonadFail.fromEither . encode . formatScientific Exponent Nothing
  toField _ _ Nothing BinaryFormat = pure . Just . BE.encodingBytes . BE.numeric
  toField backendParams encode (Just o) f | o == Oid.numeric = toField backendParams encode Nothing f
                                          | otherwise = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: Scientific"

instance HasResolution a => ToField (Fixed a) where
  toField _ encode Nothing TextFormat v = Just <$> MonadFail.fromEither (encode (show v)) -- XXX maybe slow
  toField _ _ Nothing BinaryFormat v@(MkFixed i) = pure $ Just $ BE.encodingBytes $ BE.numeric $ scientific i (fromInteger $ resolution v)
  toField backendParams encode (Just o) f v | o == Oid.numeric = toField backendParams encode Nothing f v
                                            | otherwise = fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: Fixed a (" <> show (resolution v) <> ")"

instance ToField Char where
  toField _ encode Nothing _ v = Just <$> MonadFail.fromEither (encode [v])
  toField backendParams encode (Just o) f v | o == Oid.char = toField backendParams encode Nothing f v
                                            | otherwise = fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: Char"

instance ToField String where
  toField _ encode Nothing _ = (Just <$>) . MonadFail.fromEither . encode
  toField backendParams encode (Just _) TextFormat = toField backendParams encode Nothing TextFormat
  toField backendParams encode (Just o) BinaryFormat | o `elem` [Oid.text, Oid.bpchar, Oid.varchar, Oid.name] = toField backendParams encode Nothing BinaryFormat
                                                     | otherwise = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: String"

instance ToField BS.ByteString where
  toField _ _ Nothing _ = pure . Just
  toField backendParams encode (Just o) f | o `elem` [Oid.text, Oid.bpchar, Oid.varchar, Oid.name, Oid.bytea] = toField backendParams encode Nothing f
                                          | otherwise = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: ByteString (strict)"

instance ToField Day where -- TODO infinity/-infinity
  toField _ _ Nothing TextFormat = pure . Just . BSL.toStrict . BSB.toLazyByteString . BSBP.primBounded Time.day
  toField _ _ Nothing BinaryFormat = pure . Just . BE.encodingBytes . BE.date
  toField backendParams encode (Just o) f | o == Oid.date = toField backendParams encode Nothing f
                                          | otherwise = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: Day"

instance ToField TimeOfDay where
  toField _ _ Nothing TextFormat = pure . Just . BSL.toStrict . BSB.toLazyByteString . BSBP.primBounded Time.timeOfDay
  toField backendParams _ Nothing BinaryFormat =
    case M.lookup "integer_datetimes" backendParams of
      Nothing    -> const $ fail "not found \"integer_datetimes\" backend parameter"
      Just "on"  -> pure . Just . BE.encodingBytes . BE.time_int
      Just "off" -> pure . Just . BE.encodingBytes . BE.time_float
      Just v     -> const $ fail $ "\"integer_datetimes\" has unrecognized value: " <> show v
  toField backendParams encode (Just o) f | o == Oid.time = toField backendParams encode Nothing f
                                          | otherwise = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: TimeOfDay"

instance ToField TimeOfDayWithTimeZone where
  toField _ _ Nothing TextFormat = pure . Just . BSL.toStrict . BSB.toLazyByteString . BSBP.primBounded (go BSBP.>$< Time.timeOfDay BSBP.>*< Time.timeZone)
                                   where go (TimeOfDayWithTimeZone t tz) = (t, tz)
  toField backendParams _ Nothing BinaryFormat =
    case M.lookup "integer_datetimes" backendParams of
      Nothing    -> const $ fail "not found \"integer_datetimes\" backend parameter"
      Just "on"  -> pure . Just . BE.encodingBytes . BE.timetz_int . \(TimeOfDayWithTimeZone t tz) -> (t, tz)
      Just "off" -> pure . Just . BE.encodingBytes . BE.timetz_float . \(TimeOfDayWithTimeZone t tz) -> (t, tz)
      Just v     -> const $ fail $ "\"integer_datetimes\" has unrecognized value: " <> show v
  toField backendParams encode (Just o) f | o == Oid.timetz = toField backendParams encode Nothing f
                                          | otherwise = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: TimeOfDayWithTimeZone)"

instance ToField LocalTime where
  toField _ _ Nothing TextFormat = pure . Just . BSL.toStrict . BSB.toLazyByteString . BSBP.primBounded Time.localTime
  toField _ _ Nothing BinaryFormat = pure . Just . BE.encodingBytes . BE.timestamp_int
  toField backendParams encode (Just o) f | o == Oid.timestamp = toField backendParams encode Nothing f
                                          | otherwise = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: LocalTime"

instance ToField UTCTime where
  toField _ _ Nothing TextFormat = pure . Just . BSL.toStrict . BSB.toLazyByteString . BSBP.primBounded Time.utcTime
  toField _ _ Nothing BinaryFormat = pure . Just . BE.encodingBytes . BE.timestamptz_int
  toField backendParams encode (Just o) f | o == Oid.timestamptz = toField backendParams encode Nothing f
                                          | otherwise = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: UTCTime"

instance ToField DiffTime where
  toField _ encode Nothing TextFormat = (Just <$>) . MonadFail.fromEither . encode . show -- XXX maybe slow
  toField _ _ Nothing BinaryFormat = pure . Just . BE.encodingBytes . BE.interval_int
  toField backendParams encode (Just o) f | o == Oid.interval = toField backendParams encode Nothing f
                                          | otherwise = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: DiffTime"

instance ToField NominalDiffTime where
  toField backendParams encode Nothing f = toField backendParams encode Nothing f . (realToFrac :: NominalDiffTime -> Pico)
  toField backendParams encode (Just o) f | o == Oid.numeric = toField backendParams encode Nothing f
                                          | otherwise = const $ fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: NominalDiffTime"

instance ToField Oid where
  toField _ _ Nothing TextFormat (Oid v) = pure $ Just $ BSL.toStrict $ BSB.toLazyByteString $ BSB.int32Dec v
  toField _ _ Nothing BinaryFormat (Oid v) = pure $ Just $ BE.encodingBytes $ BE.int4_int32 v
  toField backendParams encode (Just o) f v | o == Oid.oid = toField backendParams encode Nothing f v
                                            | otherwise = fail $ "type mismatch (ToField): OID: " <> show o <> ", Haskell: Oid"

-- 0 tuple
instance ToRecord () where
  toRecord _ _ Nothing [] _ =
    pure []
  toRecord _ _ Nothing fs _ =
    fail $ "the number of format codes must be 0, actually " <> show (length fs)
  toRecord _ _ (Just []) [] _ =
    pure []
  toRecord _ _ (Just os) [] _ =
    fail $ "the number of OIDs must be 0, actually " <> show (length os)
  toRecord _ _ _ fs _ =
    fail $ "the number of format codes must be 0, actually " <> show (length fs)

-- 1 tuple
instance
  {-# OVERLAPPABLE #-}
  (ToField a, Single c, t ~ c a)
  => ToRecord t where
  toRecord backendParams encode Nothing [f] (Single v) =
    sequence [toField backendParams encode Nothing f v]
  toRecord _ _ Nothing [_] _ =
    cantReachHere
  toRecord backendParams encode (Just [o]) [f] (Single v) =
    sequence [toField backendParams encode (Just o) f v]
  toRecord _ _ (Just os) [_] _ =
    fail $ "the number of OIDs must be 1, actually " <> show (length os)
  toRecord _ _ _ fs _ =
    fail $ "the number of format codes must be 1, actually " <> show (length fs)

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
instance
  {-# OVERLAPPING #-}
  ToField a
  => ToRecord [a] where
  toRecord backendParams encode Nothing fs vs =
    sequence $ uncurry (toField backendParams encode Nothing) <$> zip fs vs
  toRecord backendParams encode (Just os) fs vs =
    assert (length os == length fs && length fs == length vs) $ sequence $ uncurry3 (toField backendParams encode) <$> zip3 (Just <$> os) fs vs

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

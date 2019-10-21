{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns            #-}
{-# LANGUAGE OverloadedStrings         #-}

module Database.PostgreSQL.Pure.Internal.Exception
  ( Exception (..)
  , ErrorResponse (..)
  , ResponseParsingFailed (..)
  , InternalException (..)
  , convert
  , cantReachHere
  ) where

import           Database.PostgreSQL.Pure.Internal.Data (ErrorFields (ErrorFields), Pretty (pretty), TransactionState)

import           Control.Exception.Safe                 (displayException, fromException, throw, toException, try)
import qualified Control.Exception.Safe                 as E
import qualified Data.ByteString                        as BS
import qualified Data.ByteString.Short                  as BSS
import qualified Data.ByteString.UTF8                   as BSU
import           Data.Typeable                          (Typeable, cast)
import           GHC.Stack                              (HasCallStack)

-- | Root exception.
--
-- @
-- 'Exception'
--   ├ 'ErrorResponse'
--   └ 'ResponseParsingFailed'
-- @
data Exception = forall e. E.Exception e => Exception e deriving (Typeable)

instance Show Exception where
  show (Exception e) = show e

instance E.Exception Exception where
  displayException (Exception e) = displayException e

-- | This means that the server responds an error.
data ErrorResponse =
  ErrorResponse { severity :: BS.ByteString, code :: BS.ByteString, message :: BS.ByteString, transactionState :: Maybe TransactionState }
  deriving (Show, Read, Eq, Typeable)

instance E.Exception ErrorResponse where
  toException = toException . Exception
  fromException = ((\(Exception e) -> cast e) =<<) . fromException
  displayException = pretty

instance Pretty ErrorResponse where
  pretty ErrorResponse { severity, code, message, transactionState } =
    "error response:\n"
    <> "\tseverity: " <> BSU.toString severity -- only supports UTF-8
    <> "\n\tcode: " <> BSU.toString code
    <> "\n\tmessage: " <> BSU.toString message
    <> case transactionState of
         Just ts -> "\n\ttransaction state: " <> pretty ts
         Nothing -> mempty

-- | This means that the server responds an unknown message.
newtype ResponseParsingFailed =
  ResponseParsingFailed { causedBy :: String }
  deriving (Show, Typeable)

instance E.Exception ResponseParsingFailed where
  toException = toException . Exception
  fromException = ((\(Exception e) -> cast e) =<<) . fromException
  displayException = pretty

instance Pretty ResponseParsingFailed where
  pretty (ResponseParsingFailed c) = "response parsing failed:\n\tcaused by " <> c

data InternalException
  = InternalResponseParsingFailed String BS.ByteString
  | InternalErrorResponse ErrorFields (Maybe TransactionState)
  | InternalExtraData BS.ByteString
  deriving (Show, Read, Eq, Typeable)

instance E.Exception InternalException

internalExcepionToExposedException :: InternalException -> Exception
internalExcepionToExposedException e@InternalResponseParsingFailed {} = Exception $ ResponseParsingFailed $ displayException e
internalExcepionToExposedException (InternalErrorResponse (ErrorFields fields) transactionState) =
  Exception ErrorResponse { severity, code, message, transactionState }
  where
    (severity, code, message) = map3 BSS.fromShort $ foldr go ("", "", "") fields
    go ('S', largeS) (_, largeC, largeM) = (largeS, largeC, largeM)
    go ('C', largeC) (largeS, _, largeM) = (largeS, largeC, largeM)
    go ('M', largeM) (largeS, largeC, _) = (largeS, largeC, largeM)
    go _ a                               = a
    map3 f (v1, v2, v3) = (f v1, f v2, f v3)
internalExcepionToExposedException e@InternalExtraData {} = Exception $ ResponseParsingFailed $ displayException e

convert :: IO a -> IO a
convert a = do
  r <- try a
  case r of
    Right r -> pure r
    Left e  -> throw $ internalExcepionToExposedException e

cantReachHere :: HasCallStack => a
cantReachHere = error "can't reach here"

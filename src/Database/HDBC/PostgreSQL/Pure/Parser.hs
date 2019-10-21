{-# LANGUAGE OverloadedStrings #-}

module Database.HDBC.PostgreSQL.Pure.Parser
  ( convertQuestionMarkStyleToDollarSignStyle
  , splitQueries
  ) where

import qualified Database.PostgreSQL.Pure.Internal.Data   as Pure
import qualified Database.PostgreSQL.Pure.Internal.Parser as Pure

import           Control.Monad                            (void)
import           Control.Monad.State.Strict               (StateT, evalStateT, lift, state)
import qualified Data.Attoparsec.ByteString               as AP
import qualified Data.Attoparsec.ByteString.Char8         as APC
import           Data.Attoparsec.Combinator               ((<?>))
import qualified Data.Attoparsec.Combinator               as AP
import qualified Data.Attoparsec.Internal.Types           as API
import qualified Data.ByteString                          as BS
import qualified Data.ByteString.UTF8                     as BSU

convertQuestionMarkStyleToDollarSignStyle :: Pure.Query -> Either String Pure.Query
convertQuestionMarkStyleToDollarSignStyle (Pure.Query q) =
  flip AP.parseOnly q $
    flip evalStateT 1 $
      (Pure.Query . mconcat <$>) $ do
        ss <-
          AP.many' $
            AP.choice
              [ question
              , lift questionLiteral
              , lift singleQuoteLiteral
              , lift doubleQuoteLiteral
              , lift dollarQuoteLiteral
              , lift lineComment
              , lift blockComment
              , lift $ BS.pack . (:[]) <$> AP.anyWord8
              ]
        lift AP.endOfInput
        pure ss

singleQuoteLiteral :: AP.Parser BS.ByteString
singleQuoteLiteral = oneCharQuoteLiteral '\'' <?> "singleQuoteLiteral"

doubleQuoteLiteral :: AP.Parser BS.ByteString
doubleQuoteLiteral = oneCharQuoteLiteral '"' <?> "doubleQuoteLiteral"

oneCharQuoteLiteral :: Char -> AP.Parser BS.ByteString
oneCharQuoteLiteral quote =
  takeConsumed $ do
    void $ APC.char quote
    body
    void $ APC.char quote
  where
    body = do
      void $ APC.takeTill ((||) <$> (== '\\') <*> (== quote))
      AP.option () $ do
        void $ APC.char '\\'
        void APC.anyChar -- It continues although it is somthing wrong if the character does not stand for escape sequence.
        body

dollarQuoteLiteral :: AP.Parser BS.ByteString
dollarQuoteLiteral =
  (<?> "dollarQuoteLiteral") $
    takeConsumed $ do
      quote <- dollarQuote
      go quote
  where
    dollarQuote =
      (<?> "dollarQuote") $
        takeConsumed $ do
          void $ APC.char '$'
          void $ AP.many' $ APC.satisfy ((&&) <$> (/= ' ') <*> (/= '$'))
          void $ APC.char '$'
    go quote = go'
      where
        go' = do
          void $ AP.many' $ APC.satisfy (/= '$')
          AP.choice
            [ void $ AP.string quote
            , APC.char '$' *> go'
            ]

lineComment :: AP.Parser BS.ByteString
lineComment =
  (<?> "lineComment") $
    takeConsumed $ do
      void "--"
      void $ AP.many' $ APC.satisfy ((&&) <$> (/= '\r') <*> (/= '\n'))
      AP.option () $ void APC.endOfLine

blockComment :: AP.Parser BS.ByteString
blockComment =
  (<?> "blockComment") $
    takeConsumed $ do
      void "/*"
      go
  where
    go = do
      void $
        AP.many' $
          AP.choice
            [ void blockComment
            , void $ APC.satisfy ((&&) <$> (/= '/') <*> (/= '*'))
            ]
      AP.choice
        [ void "*/"
        , AP.lookAhead "/*" *> go
        , APC.satisfy ((||) <$> (== '/') <*> (== '*')) *> go
        ]

question :: StateT Int AP.Parser BS.ByteString
question = do
  void $ lift $ APC.char '?'
  i <- getAndIncrement
  pure $ "$" <> BSU.fromString (show i)
  where
    getAndIncrement :: StateT Int AP.Parser Int
    getAndIncrement = state (\i -> (i, i + 1))

questionLiteral :: AP.Parser BS.ByteString
questionLiteral = takeConsumed "\\?"

splitQueries :: Pure.Query -> [Pure.Query]
splitQueries query@(Pure.Query q) =
  case AP.parseOnly parser q of
    Right qs -> qs
    Left _   -> [query]
  where
    parser =
      ((Pure.Query <$>) . conv <$>) $
        AP.many' $
          AP.choice
            [ APC.char ';' >> pure Nothing
            , Just <$> singleQuoteLiteral
            , Just <$> doubleQuoteLiteral
            , Just <$> dollarQuoteLiteral
            , Just <$> lineComment
            , Just <$> blockComment
            , Just . BS.pack . (:[]) <$> AP.anyWord8
            ]
    conv :: [Maybe BS.ByteString] -> [BS.ByteString]
    conv =
      foldr go []
      where
        go (Just s) (a:as) = (s <> a):as
        go (Just s) []     = [s]
        go Nothing acc     = "":acc

takeConsumed :: AP.Parser a -> AP.Parser BS.ByteString
takeConsumed p = do
  n <-
    AP.lookAhead $ do
      API.Pos startPos <- Pure.currentPos
      void p
      API.Pos endPos <- Pure.currentPos
      pure $ endPos - startPos
  AP.take n

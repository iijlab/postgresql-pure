{-# LANGUAGE OverloadedStrings #-}

module SpecificDBTests where
import Database.HDBC
import Database.HDBC.PostgreSQL.Pure
import Database.HDBC.PostgreSQL.Pure.Parser(convertQuestionMarkStyleToDollarSignStyle)
import Test.HUnit

testp inp exp = TestCase $
    case convertQuestionMarkStyleToDollarSignStyle inp of
      Right x -> assertEqual "" exp x
      Left y -> assertFailure $ show y

tests = TestList 
        [TestLabel "empty" (testp "" ""),
         TestLabel "simple" (testp "SELECT a from b WHERE c = ?"
                                   "SELECT a from b WHERE c = $1"),
         TestLabel "multi" (testp "INSERT INTO foo VALUES (?,?)"
                                  "INSERT INTO foo VALUES ($1,$2)"),
         TestLabel "literal" (testp "INSERT INTO foo VALUES ('?', '''?')"
                                    "INSERT INTO foo VALUES ('?', '''?')"),
         TestLabel "torture" 
           (testp "-- really?\n-- yes'?\nINSERT INTO ? VALUES ('', ?, \"?asd\", '?\\'?', '?''?', /* foo? */ /* foo /* bar */ ? */ ?)"
                  "-- really?\n-- yes'?\nINSERT INTO $1 VALUES ('', $2, \"?asd\", '?\\'?', '?''?', /* foo? */ /* foo /* bar */ ? */ $3)")
           ] 
                  
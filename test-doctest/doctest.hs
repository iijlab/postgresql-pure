import           Test.DocTest (doctest)

main :: IO ()
main =
  doctest
    [ "-isrc"
    , "src/Database/PostgreSQL/Pure.hs"
    , "src/Database/PostgreSQL/Pure/List.hs"
    ]

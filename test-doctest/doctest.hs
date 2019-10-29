import           Test.DocTest (doctest)

main :: IO ()
main =
  doctest ["-isrc", "src"]

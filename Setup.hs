import           Prelude                            hiding (head, init, last, reverse, tail)
import qualified Prelude

import           Data.Char                          (isDigit)
import           Data.Foldable                      (for_)
import           Data.List                          (intercalate, intersperse, isPrefixOf, replicate, stripPrefix)
import           Distribution.Simple                (Args, UserHooks (preBuild), defaultMainWithHooks, simpleUserHooks)
import           Distribution.Simple.Setup          (BuildFlags)
import           Distribution.Types.HookedBuildInfo (HookedBuildInfo, emptyHookedBuildInfo)
import           System.Directory                   (copyFile, createDirectoryIfMissing, getModificationTime,
                                                     getTemporaryDirectory, removeFile)
import           System.FilePath                    (dropExtension, takeFileName, (</>))
import           System.IO                          (Handle, IOMode (ReadMode), hClose, hGetLine, hIsEOF, hPutStrLn,
                                                     hSetNewlineMode, noNewlineTranslation, openTempFile, stdin,
                                                     withFile)

main :: IO ()
main =
  defaultMainWithHooks
    simpleUserHooks
      { preBuild = \_ _ -> preProcessBuilder >> preProcessParser >> preProcessLength >> pure emptyHookedBuildInfo }

preProcess :: FilePath -> (Word -> [String] -> [String]) -> IO ()
preProcess srcPath embed = do
  let
    fileName = takeFileName srcPath
    templatePath = "template" </> fileName
    templateItemPath = "template" </> dropExtension fileName ++ "Item.hs"
  d <- dirty srcPath templatePath templateItemPath
  if d
    then do
      putStrLn $ "necessary to update " ++ srcPath
      tempPath <-
        withFile templatePath ReadMode $ \template -> do
          tempDir <- (</> "postgresql-pure") <$> getTemporaryDirectory
          createDirectoryIfMissing True tempDir
          (tempPath, temp) <- openTempFile tempDir fileName
          hSetNewlineMode template noNewlineTranslation
          hSetNewlineMode temp noNewlineTranslation
          hSetNewlineMode stdin noNewlineTranslation
          templateItem <- lines <$> readFile templateItemPath
          loop template temp templateItem
          hClose temp
          pure tempPath
      copyFile tempPath srcPath
      removeFile tempPath
    else putStrLn $ "unnecessary to update " ++ srcPath
  where
    loop :: Handle -> Handle -> [String] -> IO ()
    loop template temp templateItem =
      go
      where
        go = do
          eof <- hIsEOF template
          if eof
            then pure ()
            else do
              line <- hGetLine template
              for_ (preprocess line templateItem) (hPutStrLn temp)
              go

    preprocess :: String -> [String] -> [String]
    preprocess line templateItem
      | Just rest <- stripPrefix "---- embed " line
      , let n = read $ takeWhile isDigit rest
      = embed n templateItem
      | otherwise = [line]

preProcessBuilder :: IO ()
preProcessBuilder = preProcess "src/Database/PostgreSQL/Pure/Internal/Builder.hs" embed
  where
    embed :: Word -> [String] -> [String]
    embed l templateItem
      | l >= 2 = concatMap go templateItem
      | otherwise = error "length must be larger than or equal to 2"
      where
        go "" = [""]
        go t
          | Just rest <- stripPrefix "<to-field>" t = [toField ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<type-tuple>" t = [typeTuple ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<value-tuple>" t = [valueTuple ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<format-list>" t = [formatList ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<oid-list>" t = [oidList ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<to-field-nothing-list>" t = [toFieldNothingList ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<to-field-just-list>" t = [toFieldJustList ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<length>" t = [length ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<>" t = ["<>" ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<" t = error $ "unknown tag: " ++ takeWhile (/= '>') rest
          | (s, rest) <- span (/= '<') t = [s ++ Prelude.head (go rest)]
        n = fromIntegral l
        toField = paren $ take n $ ("ToField " ++) <$> i012
        typeTuple = paren $ take n i012
        valueTuple = paren $ take n v012
        formatList = bracket $ take n f012
        oidList = bracket $ take n o012
        toFieldNothingList = bracket $ take n $ (\(f, v) -> "toField backendParams encode Nothing " ++ f ++ " " ++ v) <$> zip f012 v012
        toFieldJustList = bracket $ take n $ (\(o, f, v) -> "toField backendParams encode (Just " ++ o ++ ") " ++ f ++ " " ++ v) <$> zip3 o012 f012 v012
        length = show l
        paren xs = "(" ++ intercalate ", " xs ++ ")"
        bracket xs = "[" ++ intercalate ", " xs ++ "]"
        i012 = ('i':) . show <$> [0 ..]
        o012 = ('o':) . show <$> [0 ..]
        f012 = ('f':) . show <$> [0 ..]
        v012 = ('v':) . show <$> [0 ..]

preProcessParser :: IO ()
preProcessParser = preProcess  "src/Database/PostgreSQL/Pure/Internal/Parser.hs" embed
  where
    embed :: Word -> [String] -> [String]
    embed l templateItem
      | l >= 2 = concatMap go templateItem
      | otherwise = error "length must be larger than or equal to 2"
      where
        go "" = [""]
        go t
          | Just rest <- stripPrefix "<from-field>" t = [fromField ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<tuple>" t = [tuple ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<list>" t = [list ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<length>" t = [length ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<tuple-cons>" t = [tupleCons ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<decode>" t = [decode ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<$>" t = ["<$>" ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<>" t = ["<>" ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<" t = error $ "unknown tag: " ++ takeWhile (/= '>') rest
          | (s, rest) <- span (/= '<') t = [s ++ Prelude.head (go rest)]
        n = fromIntegral l
        fromField = paren $ take n $ ("FromField " ++) <$> i012
        tuple = paren $ take n i012
        list = bracket $ take n i012
        length = show l
        tupleCons = "(" ++ replicate (n - 1) ',' ++ ")"
        decode = intercalate " <*> " $ take n $ ("column decode " ++) <$> i012
        paren xs = "(" ++ intercalate ", " xs ++ ")"
        bracket xs = "[" ++ intercalate ", " xs ++ "]"
        i012 = ('i':) . show <$> [0 ..]

preProcessLength :: IO ()
preProcessLength = preProcess "src/Database/PostgreSQL/Pure/Internal/Length.hs" embed
  where
    embed :: Word -> [String] -> [String]
    embed l templateItem
      | l >= 2 = concatMap go templateItem
      | otherwise = error "length must be larger than or equal to 2"
      where
        go "" = [""]
        go t
          | Just rest <- stripPrefix "<tuple>" t = [tuple ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<length>" t = [length ++ Prelude.head (go rest)]
          | Just rest <- stripPrefix "<" t = error $ "unknown tag: " ++ takeWhile (/= '>') rest
          | (s, rest) <- span (/= '<') t = [s ++ Prelude.head (go rest)]
        n = fromIntegral l
        tuple = paren $ take n i012
        length = show l
        paren xs = "(" ++ intercalate ", " xs ++ ")"
        i012 = ('i':) . show <$> [0 ..]

dirty :: FilePath -> FilePath -> FilePath -> IO Bool
dirty src template templateItem = do
  srcTime <- getModificationTime src
  templateTime <- getModificationTime template
  templateItemTime <- getModificationTime templateItem
  setupTime <- getModificationTime "Setup.hs"
  pure $ srcTime < maximum [templateTime, templateItemTime, setupTime]

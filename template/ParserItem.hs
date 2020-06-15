-- <length>-tuple
instance <from-field> => FromRecord <tuple> where
  fromRecord decode <list> =
    <tuple-cons> <$> <decode>
  fromRecord _ is = fail $ "length mismatch: expected <length>: actual: " <> show (length is)

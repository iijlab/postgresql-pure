-- <length>-tuple
instance <to-field> => ToRecord <type-tuple> where
  toRecord backendParams encode Nothing <format-list> <value-tuple> =
    sequence <to-field-nothing-list>
  toRecord backendParams encode (Just <oid-list>) <format-list> <value-tuple> =
    sequence <to-field-just-list>
  toRecord _ _ (Just os) _ _ =
    fail $ "the number of OIDs must be <length>, actually " <> show (length os)
  toRecord _ _ _ fs _ =
    fail $ "the number of format codes must be <length>, actually " <> show (length fs)

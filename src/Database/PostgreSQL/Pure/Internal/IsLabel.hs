{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Database.PostgreSQL.Pure.Internal.IsLabel () where

import           GHC.OverloadedLabels (IsLabel (fromLabel))
import           GHC.Records          (HasField (getField))

instance HasField x r a => IsLabel x (r -> a) where
  fromLabel = getField @x

{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Database.PostgreSQL.Pure.Internal.MonadFail
  ( fromEither
  ) where

import           Prelude            (Either (Left, Right), String, pure)

import           Control.Monad.Fail (MonadFail (fail))

instance MonadFail (Either String) where
  fail = Left

fromEither :: MonadFail m => Either String a -> m a
fromEither (Right a) = pure a
fromEither (Left e)  = fail e

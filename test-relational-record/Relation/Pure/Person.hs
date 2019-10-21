{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module Relation.Pure.Person where

import           DataSource.Pure                 (connect)

import           Prelude                         (Show)

import           Database.HDBC.Query.TH          (defineTableFromDB)
import           Database.HDBC.Schema.PostgreSQL (driverPostgreSQL)
import           GHC.Generics                    (Generic)

defineTableFromDB connect driverPostgreSQL "public" "person" [''Show, ''Generic]

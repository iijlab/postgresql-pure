{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.PostgreSQL.Pure.Internal.Length
  (Length) where

import GHC.TypeLits (Nat)
import Data.Tuple.OneTuple (OneTuple)
import Data.Tuple.Only (Only)
import Data.Functor.Identity (Identity)
import Data.Proxy (Proxy)

type family Length a :: Nat

-- 0 tuple
type instance Length () = 0

type instance Length (Proxy a) = 0

-- 1 tuple
type instance Length (Identity a) = 1

type instance Length (OneTuple a) = 1

type instance Length (Only a) = 1

---- embed 2

---- embed 3

---- embed 4

---- embed 5

---- embed 6

---- embed 7

---- embed 8

---- embed 9

---- embed 10

---- embed 11

---- embed 12

---- embed 13

---- embed 14

---- embed 15

---- embed 16

---- embed 17

---- embed 18

---- embed 19

---- embed 20

---- embed 21

---- embed 22

---- embed 23

---- embed 24

---- embed 25

---- embed 26

---- embed 27

---- embed 28

---- embed 29

---- embed 30

---- embed 31

---- embed 32

---- embed 33

---- embed 34

---- embed 35

---- embed 36

---- embed 37

---- embed 38

---- embed 39

---- embed 40

---- embed 41

---- embed 42

---- embed 43

---- embed 44

---- embed 45

---- embed 46

---- embed 47

---- embed 48

---- embed 49

---- embed 50

---- embed 51

---- embed 52

---- embed 53

---- embed 54

---- embed 55

---- embed 56

---- embed 57

---- embed 58

---- embed 59

---- embed 60

---- embed 61

---- embed 62

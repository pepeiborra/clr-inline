{-# LANGUAGE DataKinds, KindSignatures #-}

module Clr.Object where

import Data.Int
import GHC.TypeLits

type ObjectID = Int64

data Object (typ::Symbol) = Object ObjectID


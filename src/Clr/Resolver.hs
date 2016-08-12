{-# LANGUAGE                --
    DataKinds               -- Because use [] as a kind of type within the type families
  , KindSignatures          -- Because we declare how we're doing the above
  , PolyKinds
  , TypeFamilies
  , TypeOperators
  , UndecidableInstances
  #-}

module Clr.Resolver where

import Data.Kind
import GHC.TypeLits

import Data.Int



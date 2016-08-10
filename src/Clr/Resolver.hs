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

type family Concat (a::[t]) (b::[t]) :: [t] where
  Concat    '[]    ys  = ys
  Concat (x ': xs) ys  = x ': xs `Concat` ys

type family UnBridgeType (t::Type) :: [Symbol] where
  UnBridgeType String = '["System.String"]
  UnBridgeType Int32 = '["System.Int32"]
  UnBridgeType Int64 = '["System.Int64"]
  UnBridgeType (a, b) = UnBridgeType a `Concat` UnBridgeType b

type family Resolve t m (args'::Type) :: [Symbol] where
  Resolve t m args' = UnBridgeType args'


{-# LANGUAGE PolyKinds, DataKinds, TypeFamilies, UndecidableInstances #-}

module Clr.Prim where

import Clr.Common

import Data.Int(Int32)
import GHC.TypeLits

type instance SuperTypeOf "System.Object"    = 'Nothing
type instance SuperTypeOf "System.ValueType" = 'Just "System.Object"

type instance SuperTypeOf "System.String"  = 'Just "System.Object"
type instance SuperTypeOf "System.Int16"   = 'Just "System.ValueType"
type instance SuperTypeOf "System.UInt16"  = 'Just "System.ValueType"
type instance SuperTypeOf "System.Int32"   = 'Just "System.ValueType"
type instance SuperTypeOf "System.UInt32"  = 'Just "System.ValueType"
type instance SuperTypeOf "System.Int64"   = 'Just "System.ValueType"
type instance SuperTypeOf "System.UInt64"  = 'Just "System.ValueType"
type instance SuperTypeOf "System.IntPtr"  = 'Just "System.ValueType"
type instance SuperTypeOf "System.UIntPtr" = 'Just "System.ValueType"
type instance SuperTypeOf "System.Char"    = 'Just "System.ValueType"
type instance SuperTypeOf "System.Single"  = 'Just "System.ValueType"
type instance SuperTypeOf "System.Double"  = 'Just "System.ValueType"




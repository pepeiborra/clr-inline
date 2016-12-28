{-# LANGUAGE TypeInType, PolyKinds, KindSignatures, TypeFamilies, TypeOperators, UndecidableInstances, ConstraintKinds #-}

module Clr.Interface where

import Clr.ListTuple
import Clr.Types

import Data.Kind

--
-- Each .Net type can optional implement multiple interfaces
--
type family Interfaces (t::Type) :: [Type]

type family Implements (t::Type) (i::Type) :: Bool where
  Implements t i = i `Elem` (Interfaces t)

-- TODO: temporary instances here
type instance Interfaces (T "System.Object" '[])    = '[]
type instance Interfaces (T "System.ValueType" '[]) = '[]

type instance Interfaces (T "System.String"  '[]) = '[]
type instance Interfaces (T "System.SByte"   '[]) = '[]
type instance Interfaces (T "System.Byte"    '[]) = '[]
type instance Interfaces (T "System.Int16"   '[]) = '[]
type instance Interfaces (T "System.UInt16"  '[]) = '[]
type instance Interfaces (T "System.Int32"   '[]) = '[]
type instance Interfaces (T "System.UInt32"  '[]) = '[]
type instance Interfaces (T "System.Int64"   '[]) = '[]
type instance Interfaces (T "System.UInt64"  '[]) = '[]
type instance Interfaces (T "System.IntPtr"  '[]) = '[]
type instance Interfaces (T "System.UIntPtr" '[]) = '[]
type instance Interfaces (T "System.Char"    '[]) = '[]
type instance Interfaces (T "System.Single"  '[]) = '[]
type instance Interfaces (T "System.Double"  '[]) = '[]



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
type instance Interfaces (T "System.Object" 'Nothing '[])    = '[]
type instance Interfaces (T "System.ValueType" 'Nothing '[]) = '[]

type instance Interfaces (T "System.String"  'Nothing '[]) = '[]
type instance Interfaces (T "System.SByte"   'Nothing '[]) = '[]
type instance Interfaces (T "System.Byte"    'Nothing '[]) = '[]
type instance Interfaces (T "System.Int16"   'Nothing '[]) = '[]
type instance Interfaces (T "System.UInt16"  'Nothing '[]) = '[]
type instance Interfaces (T "System.Int32"   'Nothing '[]) = '[]
type instance Interfaces (T "System.UInt32"  'Nothing '[]) = '[]
type instance Interfaces (T "System.Int64"   'Nothing '[]) = '[]
type instance Interfaces (T "System.UInt64"  'Nothing '[]) = '[]
type instance Interfaces (T "System.IntPtr"  'Nothing '[]) = '[]
type instance Interfaces (T "System.UIntPtr" 'Nothing '[]) = '[]
type instance Interfaces (T "System.Char"    'Nothing '[]) = '[]
type instance Interfaces (T "System.Single"  'Nothing '[]) = '[]
type instance Interfaces (T "System.Double"  'Nothing '[]) = '[]



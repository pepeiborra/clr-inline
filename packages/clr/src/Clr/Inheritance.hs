{-# LANGUAGE TypeInType, PolyKinds, TypeFamilies  #-}
{-# LANGUAGE UndecidableInstances, TypeOperators #-}

module Clr.Inheritance where

import Clr.ListTuple
import Clr.Types
import Clr.Object

import Data.Kind
import Data.Type.Equality
import Data.Type.Bool
import GHC.TypeLits
import Unsafe.Coerce

--
-- Most types inherit from another type. Interfaces inherit from multiple types.
--
type family SuperTypes (t::Type) :: [Type]

type family HasSuper (t::Type) :: Bool where
  HasSuper t = ((ListSize (SuperTypes t)) `CmpNat` 0) == GT

type family SuperTypesL (ts::[Type]) :: [[Type]] where
  SuperTypesL   '[]     = '[ '[] ]
  SuperTypesL (x ': xs) = (SuperTypes x) ': (SuperTypesL xs)
--
-- A list of t plus all types that t implements or inherits from
--
type family WithAllSuperTypes (t::Type) :: [Type] where
  WithAllSuperTypes t = Concat (WithAllSuperTypes' '[t])

type family WithAllSuperTypes' (ts::[Type]) :: [[Type]] where
  WithAllSuperTypes'    '[]    = '[]
  WithAllSuperTypes' (x ': xs) = '[x] ': (SuperTypes x) ': (WithAllSuperTypes' (Concat (SuperTypesL (SuperTypes x)) `Append` xs))
--
-- t1 `Implements` t2 is True when t1 is equal to t2, t1 inherits from the base type t2, or t1 implements the interface t2
--
type family Implements (t1 :: Type) (t2 :: Type) :: Bool where
  Implements t1 t1 = 'True
  Implements t1 t2 = t2 `Elem` (WithAllSuperTypes t1)

--
-- Value types are just those that inherit from System.ValueType
--
type family IsValueType (a::Type) :: Bool where
  IsValueType a = a `Implements` (T "System.ValueType" '[])

--
-- Reference types are those that do not inherit fom System.ValueType
--
type family IsRefType (a::Type) :: Bool where
  IsRefType a = Not (IsValueType a)

--
-- Important SuperType declarations
--
type instance SuperTypes (T "System.Object" '[])    = '[]
type instance SuperTypes (T "System.ValueType" '[]) = '[ (T "System.Object" '[]) ]

--
-- SuperType declarations for each prim type
--
type instance SuperTypes (T "System.String"  '[]) = '[ (T "System.Object"    '[]) ]
type instance SuperTypes (T "System.SByte"   '[]) = '[ (T "System.ValueType" '[]) ]
type instance SuperTypes (T "System.Byte"    '[]) = '[ (T "System.ValueType" '[]) ]
type instance SuperTypes (T "System.Int16"   '[]) = '[ (T "System.ValueType" '[]) ]
type instance SuperTypes (T "System.UInt16"  '[]) = '[ (T "System.ValueType" '[]) ]
type instance SuperTypes (T "System.Int32"   '[]) = '[ (T "System.ValueType" '[]) ]
type instance SuperTypes (T "System.UInt32"  '[]) = '[ (T "System.ValueType" '[]) ]
type instance SuperTypes (T "System.Int64"   '[]) = '[ (T "System.ValueType" '[]) ]
type instance SuperTypes (T "System.UInt64"  '[]) = '[ (T "System.ValueType" '[]) ]
type instance SuperTypes (T "System.IntPtr"  '[]) = '[ (T "System.ValueType" '[]) ]
type instance SuperTypes (T "System.UIntPtr" '[]) = '[ (T "System.ValueType" '[]) ]
type instance SuperTypes (T "System.Char"    '[]) = '[ (T "System.ValueType" '[]) ]
type instance SuperTypes (T "System.Single"  '[]) = '[ (T "System.ValueType" '[]) ]
type instance SuperTypes (T "System.Double"  '[]) = '[ (T "System.ValueType" '[]) ]

--
-- Casting up the hierarchy. Always safe
--
upCast :: (t `Implements` t' ~ 'True ) => Object t -> Object t'
upCast = unsafeCoerce

--
-- Casting down the hierarchy. TODO: runtime checks
--
unsafeDownCast :: (t' `Implements` t ~ 'True ) => Object t -> Object t'
unsafeDownCast = unsafeCoerce

--
-- When a method m is invoked on a type t, we need to go up the hierarchy
-- to find the type that t derives from that declared m
--
type family ResolveBaseType (t::Type) (m::Type) :: Type where
  ResolveBaseType t m = ResolveBaseType' (WithAllSuperTypes t) m

type family ResolveBaseType' (ts::[Type]) (m::Type) :: Type where
  ResolveBaseType'   '[]     m = TypeError (Text "Could not find declaring base type of member " :<>: ShowType m)
  ResolveBaseType' (t ': ts) m = If (t `HasMember` m) t (ResolveBaseType' ts m)



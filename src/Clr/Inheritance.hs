{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, TypeOperators #-}

module Clr.Inheritance where

import Clr.ListTuple
import Clr.Object

import GHC.TypeLits
import Data.Kind
import Data.Type.Equality
import Data.Type.Bool
import Unsafe.Coerce

--
-- Most types inherit from another type
--
type family SuperTypeOf (t::Type) :: Maybe Type


--
-- A list of all types that t inherits from
--
type family SuperTypesOf (t :: Type) :: [Type] where
  SuperTypesOf x = CatMaybes (SuperTypesOf' ('Just x))

type family SuperTypesOf' (t :: Maybe Type) :: [Maybe Type] where
  SuperTypesOf' ('Just x) = (SuperTypeOf x) ': (SuperTypesOf' (SuperTypeOf x))
  SuperTypesOf' 'Nothing  = '[]

--
-- True if t1 is equal to or inherits from t2
--
type family InheritsFrom (t1 :: Type) (t2 :: Type) :: Bool where
  InheritsFrom t1 t2 = t1 == t2 || Elem t2 (SuperTypesOf t1)

--
-- Value types are just those that inherit from System.ValueType
--
type family IsValueType (a::Type) :: Bool where
  IsValueType a = a `InheritsFrom` (ClrType "System.ValueType" '[])

--
-- Prim types are all values types + Sytem.String
--
type family IsPrimType (a::Type) :: Bool where
  IsPrimType (ClrType "System.String" '[])  = 'True
  IsPrimType (ClrType "System.Int16" '[])   = 'True
  IsPrimType (ClrType "System.UInt16" '[])  = 'True
  IsPrimType (ClrType "System.Int32" '[])   = 'True
  IsPrimType (ClrType "System.UInt32" '[])  = 'True
  IsPrimType (ClrType "System.Int64" '[])   = 'True
  IsPrimType (ClrType "System.UInt64" '[])  = 'True
  IsPrimType (ClrType "System.IntPtr" '[])  = 'True
  IsPrimType (ClrType "System.UIntPtr" '[]) = 'True
  IsPrimType (ClrType "System.Char" '[])    = 'True
  IsPrimType (ClrType "System.Single" '[])  = 'True
  IsPrimType (ClrType "System.Double" '[])  = 'True
  IsPrimType t = 'False
  -- IsPrimType  a                            = IsValueType a

--
-- Reference types are those that do not inherit fom System.ValueType
--
type family IsRefType (a::Type) :: Bool where
  IsRefType a = Not (IsValueType a)

--
-- Important SuperType declarations
--
type instance SuperTypeOf (ClrType "System.Object" '[])    = 'Nothing
type instance SuperTypeOf (ClrType "System.ValueType" '[]) = 'Just (ClrType "System.Object" '[])

--
-- SuperType declarations for each prim type
--
type instance SuperTypeOf (ClrType "System.String" '[])  = 'Just (ClrType "System.Object" '[])
type instance SuperTypeOf (ClrType "System.Int16" '[])   = 'Just (ClrType "System.ValueType" '[])
type instance SuperTypeOf (ClrType "System.UInt16" '[])  = 'Just (ClrType "System.ValueType" '[])
type instance SuperTypeOf (ClrType "System.Int32" '[])   = 'Just (ClrType "System.ValueType" '[])
type instance SuperTypeOf (ClrType "System.UInt32" '[])  = 'Just (ClrType "System.ValueType" '[])
type instance SuperTypeOf (ClrType "System.Int64" '[])   = 'Just (ClrType "System.ValueType" '[])
type instance SuperTypeOf (ClrType "System.UInt64" '[])  = 'Just (ClrType "System.ValueType" '[])
type instance SuperTypeOf (ClrType "System.IntPtr" '[])  = 'Just (ClrType "System.ValueType" '[])
type instance SuperTypeOf (ClrType "System.UIntPtr" '[]) = 'Just (ClrType "System.ValueType" '[])
type instance SuperTypeOf (ClrType "System.Char" '[])    = 'Just (ClrType "System.ValueType" '[])
type instance SuperTypeOf (ClrType "System.Single" '[])  = 'Just (ClrType "System.ValueType" '[])
type instance SuperTypeOf (ClrType "System.Double" '[])  = 'Just (ClrType "System.ValueType" '[])

--
-- Casting up the hierarchy. Always safe
--
upCast :: (t `InheritsFrom` t' ~ True ) => Object t -> Object t'
upCast = unsafeCoerce

--
-- Casting down the hierarchy. TODO: runtime checks
--
unsafeDownCast :: (t' `InheritsFrom` t ~ True ) => Object t -> Object t'
unsafeDownCast = unsafeCoerce


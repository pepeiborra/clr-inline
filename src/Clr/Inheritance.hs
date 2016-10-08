{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, TypeOperators #-}

module Clr.Inheritance where

import Clr.ListTuple
import Clr.Types
import Clr.Object

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
  IsValueType a = a `InheritsFrom` (T "System.ValueType" '[])

--
-- Reference types are those that do not inherit fom System.ValueType
--
type family IsRefType (a::Type) :: Bool where
  IsRefType a = Not (IsValueType a)

--
-- Important SuperType declarations
--
type instance SuperTypeOf (T "System.Object" '[])    = 'Nothing
type instance SuperTypeOf (T "System.ValueType" '[]) = 'Just (T "System.Object" '[])

--
-- SuperType declarations for each prim type
--
type instance SuperTypeOf (T "System.String" '[])  = 'Just (T "System.Object" '[])
type instance SuperTypeOf (T "System.Int16" '[])   = 'Just (T "System.ValueType" '[])
type instance SuperTypeOf (T "System.UInt16" '[])  = 'Just (T "System.ValueType" '[])
type instance SuperTypeOf (T "System.Int32" '[])   = 'Just (T "System.ValueType" '[])
type instance SuperTypeOf (T "System.UInt32" '[])  = 'Just (T "System.ValueType" '[])
type instance SuperTypeOf (T "System.Int64" '[])   = 'Just (T "System.ValueType" '[])
type instance SuperTypeOf (T "System.UInt64" '[])  = 'Just (T "System.ValueType" '[])
type instance SuperTypeOf (T "System.IntPtr" '[])  = 'Just (T "System.ValueType" '[])
type instance SuperTypeOf (T "System.UIntPtr" '[]) = 'Just (T "System.ValueType" '[])
type instance SuperTypeOf (T "System.Char" '[])    = 'Just (T "System.ValueType" '[])
type instance SuperTypeOf (T "System.Single" '[])  = 'Just (T "System.ValueType" '[])
type instance SuperTypeOf (T "System.Double" '[])  = 'Just (T "System.ValueType" '[])

--
-- Casting up the hierarchy. Always safe
--
upCast :: (t `InheritsFrom` t' ~ 'True ) => Object t -> Object t'
upCast = unsafeCoerce

--
-- Casting down the hierarchy. TODO: runtime checks
--
unsafeDownCast :: (t' `InheritsFrom` t ~ 'True ) => Object t -> Object t'
unsafeDownCast = unsafeCoerce


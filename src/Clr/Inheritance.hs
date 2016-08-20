{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, TypeOperators #-}

module Clr.Inheritance where

import Clr.ListTuple
import Clr.Object

import GHC.TypeLits
import Data.Type.Equality
import Data.Type.Bool
import Unsafe.Coerce

--
-- Most types inherit from another type
--
type family SuperTypeOf (t::Symbol) :: Maybe Symbol


--
-- A list of all types that t inherits from
--
type family SuperTypesOf (t :: Symbol) :: [Symbol] where
  SuperTypesOf x = UnMaybeList (SuperTypesOf' ('Just x))

type family SuperTypesOf' (t :: Maybe Symbol) :: [Maybe Symbol] where
  SuperTypesOf' ('Just x) = (SuperTypeOf x) ': (SuperTypesOf' (SuperTypeOf x))
  SuperTypesOf' 'Nothing  = '[]

--
-- True if t1 is equal to or inherits from t2
--
type family InheritsFrom (t1 :: Symbol) (t2 :: Symbol) :: Bool where
  InheritsFrom t1 t2 = t1 == t2 || Elem t2 (SuperTypesOf t1)

--
-- Value types are just those that inherit from System.ValueType
--
type family IsValueType (a::Symbol) :: Bool where
  IsValueType a = a `InheritsFrom` "System.ValueType"

--
-- Prim types are all values types + Sytem.String
--
type family IsPrimType (a::Symbol) :: Bool where
  IsPrimType "System.String" = True
  IsPrimType  a              = IsValueType a

--
-- Reference types are those that do not inherit fom System.ValueType
--
type family IsRefType (a::Symbol) :: Bool where
  IsRefType a = Not (IsValueType a)

--
-- Important SuperType declarations
--
type instance SuperTypeOf "System.Object"    = 'Nothing
type instance SuperTypeOf "System.ValueType" = 'Just "System.Object"

--
-- SuperType declarations for each prim type
--
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


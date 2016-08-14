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

type family SuperTypeOf (t::Symbol) :: Maybe Symbol

type family SuperTypesOf' (t :: Maybe Symbol) :: [Maybe Symbol] where
  SuperTypesOf' ('Just x) = (SuperTypeOf x) ': (SuperTypesOf' (SuperTypeOf x))
  SuperTypesOf' 'Nothing  = '[]

type family SuperTypesOf (t :: Symbol) :: [Symbol] where
  SuperTypesOf x = UnMaybeList (SuperTypesOf' ('Just x))

type family InheritsFrom (t1 :: Symbol) (t2 :: Symbol) :: Bool where
  InheritsFrom t1 t2 = t1 == t2 || Elem t2 (SuperTypesOf t1)

type family IsValueType (a::Symbol) :: Bool where
  IsValueType a = a `InheritsFrom` "System.ValueType"

type family IsPrimType (a::Symbol) :: Bool where
  IsPrimType "System.String" = True
  IsPrimType  a              = IsValueType a

type family IsRefType (a::Symbol) :: Bool where
  IsRefType a = Not (IsValueType a)

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

upCast :: (t `InheritsFrom` t' ~ True ) => Object t -> Object t'
upCast = unsafeCoerce

unsafeDownCast :: (t' `InheritsFrom` t ~ True ) => Object t -> Object t'
unsafeDownCast = unsafeCoerce


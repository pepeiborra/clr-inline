{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, TypeFamilies, UndecidableInstances #-}

module Clr.Common where

import GHC.TypeLits
import Data.Int(Int64)
import Data.Kind
import Data.Type.Bool
import Data.Type.Equality

type ObjectID = Int64

data Object (typ::Symbol) = Object ObjectID

type family SuperTypeOf (t::Symbol) :: Maybe Symbol

type family SuperTypesOf' (t :: Maybe Symbol) :: [Maybe Symbol] where
  SuperTypesOf' ('Just x) = (SuperTypeOf x) ': (SuperTypesOf' (SuperTypeOf x))
  SuperTypesOf' 'Nothing  = '[]

type family SuperTypesOf (t :: Symbol) :: [Symbol] where
  SuperTypesOf x = UnMaybeList (SuperTypesOf' ('Just x))

type family UnMaybeList (l :: [Maybe k]) :: [k] where
  UnMaybeList '[]             = '[]
  UnMaybeList ('Just x ': xs)  = x ': UnMaybeList xs
  UnMaybeList ('Nothing ': xs) = UnMaybeList xs

type family Elem (a :: k) (xs::[k]) :: Bool where
  Elem a    '[]    = 'False
  Elem a (x ': xs) = a == x || (Elem a xs)

type family InheritsFrom (t1 :: Symbol) (t2 :: Symbol) :: Bool where
  InheritsFrom t1 t2 = t1 == t2 || Elem t2 (SuperTypesOf t1)

type family IsValueType (a::Symbol) :: Bool where
  IsValueType a = a `InheritsFrom` "System.ValueType"

type family IsPrimType (a::Symbol) :: Bool where
  IsPrimType "System.String" = True
  IsPrimType  a              = IsValueType a

type family IsRefType (a::Symbol) :: Bool where
  IsRefType a = Not (IsValueType a)

type family ListToTuple (t :: [Type]) :: Type where
    ListToTuple '[]                                 = ()
    ListToTuple (a ': '[])                         = a
    ListToTuple (a ': b ': '[])                   = (a,b)
    ListToTuple (a ': b ': c ': '[])             = (a,b,c)
    ListToTuple (a ': b ': c ': d ': '[])       = (a,b,c,d)
    ListToTuple (a ': b ': c ': d ': e ': '[]) = (a,b,c,d,e)


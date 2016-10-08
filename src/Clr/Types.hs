{-# LANGUAGE KindSignatures, TypeInType, TypeFamilies, TypeOperators #-}

module Clr.Types where

import Data.Kind
import GHC.TypeLits

--
-- A type in the Clr is its name plus zero or more other types that it is instantiated with (generics)
--
data T (name::Symbol) (genArgs::[Type])

--
-- Simplifies contruction of a ClrType from just the name
--
type family ToClrType (x::k) :: Type where
  ToClrType (name::Symbol) = T name '[]
  ToClrType (T name gt)    = T name  gt

--
-- Like above, but works on a list
--
type family ToClrTypeL (ts::[k]) :: [Type] where
  ToClrTypeL (x ': '[]) = '[ToClrType x]
  ToClrTypeL (x ':  xs) = (ToClrType x) ': (ToClrTypeL xs)

--
-- Simplifies contruction of generic type as the list may be symbols
--
type family GenT (name::Symbol) (xs::k) :: Type where
  GenT (name::Symbol) xs = T name (ToClrTypeL xs)

--
-- Prim types are all values types + Sytem.String
--
type family IsPrimType (a::Type) :: Bool where
  IsPrimType (T "System.String" '[])  = 'True
  IsPrimType (T "System.Int16" '[])   = 'True
  IsPrimType (T "System.UInt16" '[])  = 'True
  IsPrimType (T "System.Int32" '[])   = 'True
  IsPrimType (T "System.UInt32" '[])  = 'True
  IsPrimType (T "System.Int64" '[])   = 'True
  IsPrimType (T "System.UInt64" '[])  = 'True
  IsPrimType (T "System.IntPtr" '[])  = 'True
  IsPrimType (T "System.UIntPtr" '[]) = 'True
  IsPrimType (T "System.Char" '[])    = 'True
  IsPrimType (T "System.Single" '[])  = 'True
  IsPrimType (T "System.Double" '[])  = 'True
  IsPrimType t = 'False


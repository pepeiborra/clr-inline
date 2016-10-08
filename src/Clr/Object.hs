{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeInType, TypeOperators, TypeFamilies #-}

module Clr.Object where

import Data.Kind
import Data.Int
import GHC.TypeLits
import Clr.ListTuple

--
-- A unique indentifier for a particular ref type.
-- Just a place holder for now might need to something else such as pointer later on
--
type ObjectID = Int64

--
-- An object is just its unique identifer + information of its type
--
data Object (typ::Type) where
  Object :: ObjectID -> Object typ

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

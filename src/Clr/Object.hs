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
-- A type in the Clr is its name plus ero or more other types that it is instantiated with (generics)
--
data ClrType (name::Symbol) (genArgs::[Type])

--
-- Simple construction of a non generic type
--
type family T (name::Symbol) where
  T name = ClrType name '[]

--
-- Construction of a generic type
--
type family GT (name::Symbol) (gt::[Type]) where
  GT name gt = ClrType name gt


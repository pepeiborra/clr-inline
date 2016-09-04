{-# LANGUAGE DataKinds, KindSignatures, GADTs, TypeInType #-}

module Clr.Object where

import Data.Kind
import Data.Int
import GHC.TypeLits

--
-- A unique indentifier for a particular ref type.
-- Just a place holder for now might need to something else such as pointer later on
--
type ObjectID = Int64

data ClrType (name::Symbol) (genArgs::[Type])

--
-- An object is just its unique identifer + information of its type
--
data Object (typ::Type) where
  Object :: ObjectID -> Object typ


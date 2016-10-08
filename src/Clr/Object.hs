{-# LANGUAGE KindSignatures, GADTs, TypeInType #-}

module Clr.Object where

import Data.Kind
import Data.Int

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


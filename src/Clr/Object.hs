{-# LANGUAGE DataKinds, KindSignatures #-}

module Clr.Object where

import Data.Int
import GHC.TypeLits

--
-- A unique indentifier for a particular ref type.
-- Just a place holder for now might need to something else such as pointer later on
--
type ObjectID = Int64

--
-- An object is just its unique indentifer with extra type information
--
data Object (typ::Symbol) = Object ObjectID


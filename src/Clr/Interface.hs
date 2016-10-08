{-# LANGUAGE TypeInType, PolyKinds, KindSignatures, TypeFamilies, TypeOperators, UndecidableInstances, ConstraintKinds #-}

module Clr.Interface where

import Clr.ListTuple
import Clr.Types

import Data.Kind

--
-- Each .Net type can optional implement multiple interfaces
--
type family Interfaces (t::Type) :: [Type]

type family Implements (t::Type) (i::Type) :: Bool where
  Implements t i = i `Elem` (Interfaces t)



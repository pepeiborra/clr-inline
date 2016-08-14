{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, TypeFamilies, UndecidableInstances #-}

module Clr.Bridge where

import Clr.Inheritance
import Clr.Object
import Clr.ListTuple

import Data.Kind
import Data.Type.Bool
import GHC.TypeLits

import Data.Int
import Data.Word
import Foreign.C
import Foreign.Ptr


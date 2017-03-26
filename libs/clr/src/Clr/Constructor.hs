{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr.Constructor
  ( Constructor(..)
  , Constructor1(..)
  , Constructor2(..)
  , Constructor3(..)
  ) where

import Clr.Bridge
import Clr.Curry

import GHC.TypeLits
import Data.Kind

--
-- Constructors
--
class Constructor1 (t::Type) (arg0::Type) where
  rawNew1 :: (BridgeType arg0) -> (IO (BridgeType t))

class Constructor2 (t::Type) (arg0::Type) (arg1::Type) where
  rawNew2 :: (BridgeType arg0) -> (BridgeType arg1) -> (IO (BridgeType t))

class Constructor3 (t::Type) (arg0::Type) (arg1::Type) (arg3::Type) where
  rawNew3 :: (BridgeType arg0) -> (BridgeType arg1) -> (BridgeType arg3) -> (IO (BridgeType t))

--
-- Unification of constructors
--

class Constructor (n::Nat) (t::Type) (args::[Type]) where
  rawNew :: CurryT' n (BridgeTypes args) (IO (BridgeType t))

instance (Constructor1 t ()) => Constructor 1 t '[] where
  rawNew = rawNew1 @t @()

instance (Constructor1 t a) => Constructor 1 t '[a] where
  rawNew = rawNew1 @t @a

instance (Constructor2 t a0 a1) => Constructor 2 t '[a0, a1] where
  rawNew = rawNew2 @t @a0 @a1

instance (Constructor3 t a0 a1 a2) => Constructor 3 t '[a0, a1, a2] where
  rawNew = rawNew3 @t @a0 @a1 @a2


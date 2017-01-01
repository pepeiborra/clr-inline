{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr.InstanceMethod
  ( MethodI(..)
  , MethodI1(..)
  , MethodI2(..)
  , MethodI3(..)
  ) where

import Clr.Bridge
import Clr.Curry

import GHC.TypeLits
import Data.Kind

--
-- Instance methods
--

class MethodI1 (t::Type) (m::Type) (arg0::Type) where
  type ResultTypeI1 t m arg0 :: Maybe Type
  rawInvokeI1 :: (BridgeType t) -> (BridgeType arg0) -> (IO (BridgeTypeM (ResultTypeI1 t m arg0)))

class MethodI2 (t::Type) (m::Type) (arg0::Type) (arg1::Type) where
  type ResultTypeI2 t m arg0 arg1 :: Maybe Type
  rawInvokeI2 :: (BridgeType t) -> (BridgeType arg0) -> (BridgeType arg1) -> (IO (BridgeTypeM (ResultTypeI2 t m arg0 arg1)))

class MethodI3 (t::Type) (m::Type) (arg0::Type) (arg1::Type) (arg2::Type) where
  type ResultTypeI3 t m arg0 arg1 arg2 :: Maybe Type
  rawInvokeI3 :: (BridgeType t) -> (BridgeType arg0) -> (BridgeType arg1) -> (BridgeType arg2) -> (IO (BridgeTypeM (ResultTypeI3 t m arg0 arg1 arg2)))

--
-- Unification of instance methods
--

class MethodI (n::Nat) (t::Type) (m::Type) (args::[Type]) where
  type ResultTypeI n t m args :: Maybe Type
  rawInvokeI :: (BridgeType t) -> CurryT' n (BridgeTypes args) (IO (BridgeTypeM (ResultTypeI n t m args)))

instance (MethodI1 t m ()) => MethodI 1 t m '[] where
  type ResultTypeI 1 t m '[] = ResultTypeI1 t m ()
  rawInvokeI = rawInvokeI1 @t @m @()

instance (MethodI1 t m a) => MethodI 1 t m '[a] where
  type ResultTypeI 1 t m '[a] = ResultTypeI1 t m a
  rawInvokeI = rawInvokeI1 @t @m @a

instance (MethodI2 t m a0 a1) => MethodI 2 t m '[a0, a1] where
  type ResultTypeI 2 t m '[a0, a1] = ResultTypeI2 t m a0 a1
  rawInvokeI = rawInvokeI2 @t @m @a0 @a1

instance (MethodI3 t m a0 a1 a2) => MethodI 3 t m '[a0, a1, a2] where
  type ResultTypeI 3 t m '[a0, a1, a2] = ResultTypeI3 t m a0 a1 a2
  rawInvokeI = rawInvokeI3 @t @m @a0 @a1 @a2


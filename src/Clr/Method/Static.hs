{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr.Method.Static
  ( MethodS(..)
  , MethodS1(..)
  , MethodS2(..)
  , MethodS3(..)
  ) where

import Clr.Bridge
import Clr.Curry

import GHC.TypeLits
import Data.Kind


--
-- Static methods
--

class MethodS1 (t::Type) (m::Type) (arg0::Type) where
  type ResultTypeS1 t m arg0 :: Maybe Type
  rawInvokeS1 :: (BridgeType arg0) -> (IO (BridgeTypeM (ResultTypeS1 t m arg0)))

class MethodS2 (t::Type) (m::Type) (arg0::Type) (arg1::Type) where
  type ResultTypeS2 t m arg0 arg1 :: Maybe Type
  rawInvokeS2 :: (BridgeType arg0) -> (BridgeType arg1) -> (IO (BridgeTypeM (ResultTypeS2 t m arg0 arg1)))

class MethodS3 (t::Type) (m::Type) (arg0::Type) (arg1::Type) (arg2::Type) where
  type ResultTypeS3 t m arg0 arg1 arg2 :: Maybe Type
  rawInvokeS3 :: (BridgeType arg0) -> (BridgeType arg1) -> (BridgeType arg2) -> (IO (BridgeTypeM (ResultTypeS3 t m arg0 arg1 arg2)))

--
-- Unification of static methods
--

class MethodS (n::Nat) (t::Type) (m::Type) (args::[Type]) where
  type ResultTypeS n t m args :: Maybe Type
  rawInvokeS :: CurryT' n (BridgeTypes args) (IO (BridgeTypeM (ResultTypeS n t m args)))

instance (MethodS1 t m ()) => MethodS 1 t m '[] where
  type ResultTypeS 1 t m '[] = ResultTypeS1 t m ()
  rawInvokeS = rawInvokeS1 @t @m @()

instance (MethodS1 t m a) => MethodS 1 t m '[a] where
  type ResultTypeS 1 t m '[a] = ResultTypeS1 t m a
  rawInvokeS = rawInvokeS1 @t @m @a

instance (MethodS2 t m a0 a1) => MethodS 2 t m '[a0, a1] where
  type ResultTypeS 2 t m '[a0, a1] = ResultTypeS2 t m a0 a1
  rawInvokeS = rawInvokeS2 @t @m @a0 @a1

instance (MethodS3 t m a0 a1 a2) => MethodS 3 t m '[a0, a1, a2] where
  type ResultTypeS 3 t m '[a0, a1, a2] = ResultTypeS3 t m a0 a1 a2
  rawInvokeS = rawInvokeS3 @t @m @a0 @a1 @a2


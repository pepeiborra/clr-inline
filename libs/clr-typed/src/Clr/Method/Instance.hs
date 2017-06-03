{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr.Method.Instance
  ( MethodI(..)
  , MethodResultI1(..)
  , MethodResultI2(..)
  , MethodResultI3(..)
  , MethodInvokeI1(..)
  , MethodInvokeI2(..)
  , MethodInvokeI3(..)
  , invokeI
  ) where

import Clr.Bridge
import Clr.Curry
import Clr.Inheritance
import Clr.ListTuple
import Clr.Marshal
import Clr.Object
import Clr.Resolver
import Clr.Types
import Clr.UnmarshalAs

import GHC.TypeLits
import Data.Kind

--
-- Instance methods
--

class MethodResultI1 (t::Type) (m::Type) (arg0::Type) where
  type ResultTypeI1 t m arg0 :: Maybe Type

class MethodResultI2 (t::Type) (m::Type) (arg0::Type) (arg1::Type) where
  type ResultTypeI2 t m arg0 arg1 :: Maybe Type

class MethodResultI3 (t::Type) (m::Type) (arg0::Type) (arg1::Type) (arg2::Type) where
  type ResultTypeI3 t m arg0 arg1 arg2 :: Maybe Type

class MethodResultI1 (t::Type) (m::Type) (arg0::Type)
   => MethodInvokeI1 (t::Type) (m::Type) (arg0::Type) where
  rawInvokeI1 :: (BridgeType t) -> (BridgeType arg0) -> (IO (BridgeTypeM (ResultTypeI1 t m arg0)))

class MethodResultI2 (t::Type) (m::Type) (arg0::Type) (arg1::Type)
   => MethodInvokeI2 (t::Type) (m::Type) (arg0::Type) (arg1::Type) where
  rawInvokeI2 :: (BridgeType t) -> (BridgeType arg0) -> (BridgeType arg1) -> (IO (BridgeTypeM (ResultTypeI2 t m arg0 arg1)))

class MethodResultI3 (t::Type) (m::Type) (arg0::Type) (arg1::Type) (arg2::Type)
   => MethodInvokeI3 (t::Type) (m::Type) (arg0::Type) (arg1::Type) (arg2::Type) where
  rawInvokeI3 :: (BridgeType t) -> (BridgeType arg0) -> (BridgeType arg1) -> (BridgeType arg2) -> (IO (BridgeTypeM (ResultTypeI3 t m arg0 arg1 arg2)))

--
-- Unification of instance methods
--

class MethodI (n::Nat) (t::Type) (m::Type) (args::[Type]) where
  type ResultTypeI n t m args :: Maybe Type
  rawInvokeI :: (BridgeType t) -> CurryT' n (BridgeTypes args) (IO (BridgeTypeM (ResultTypeI n t m args)))

instance (MethodInvokeI1 t m ()) => MethodI 1 t m '[] where
  type ResultTypeI 1 t m '[] = ResultTypeI1 t m ()
  rawInvokeI = rawInvokeI1 @t @m @()

instance (MethodInvokeI1 t m a) => MethodI 1 t m '[a] where
  type ResultTypeI 1 t m '[a] = ResultTypeI1 t m a
  rawInvokeI = rawInvokeI1 @t @m @a

instance (MethodInvokeI2 t m a0 a1) => MethodI 2 t m '[a0, a1] where
  type ResultTypeI 2 t m '[a0, a1] = ResultTypeI2 t m a0 a1
  rawInvokeI = rawInvokeI2 @t @m @a0 @a1

instance (MethodInvokeI3 t m a0 a1 a2) => MethodI 3 t m '[a0, a1, a2] where
  type ResultTypeI 3 t m '[a0, a1, a2] = ResultTypeI3 t m a0 a1 a2
  rawInvokeI = rawInvokeI3 @t @m @a0 @a1 @a2

--
-- API
--

invokeI :: forall ms m tBase tDerived argsClrUnResolved argsClr argsHask argCount argsBridge resultBridge resultHask .
            ( MakeT ms ~ m
            , ArgCount argsHask ~ argCount
            , ResolveBaseType tDerived m ~ tBase
            , tDerived `Implements` tBase ~ 'True
            , HaskToClrL (TupleToList argsHask) ~ argsClrUnResolved
            , ResolveMember argsClrUnResolved (Candidates tBase m) ~ argsClr
            , MethodI argCount tBase m argsClr
            , ListToTuple (BridgeTypeL argsClr) ~ argsBridge
            , BridgeTypeM (ResultTypeI argCount tBase m argsClr) ~ resultBridge
            , Marshal argsHask argsBridge
            , Marshal (Object tBase) (BridgeType tBase)
            , UnmarshalAs resultBridge ~ resultHask
            , Unmarshal resultBridge resultHask
            , Curry argCount (argsBridge -> IO resultBridge) (CurryT' argCount argsBridge (IO resultBridge))
            ) => Object tDerived -> argsHask -> IO resultHask
invokeI obj x = marshal @argsHask @argsBridge @resultBridge x (\tup-> marshal @(Object tBase) @(BridgeType tBase) @resultBridge (upCast obj) (\obj'-> uncurryN @argCount (rawInvokeI @argCount @tBase @m @argsClr obj') tup)) >>= unmarshal


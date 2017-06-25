{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr.Method.Static
  ( MethodS(..)
  , MethodResultS1(..)
  , MethodResultS2(..)
  , MethodResultS3(..)
  , MethodInvokeS1(..)
  , MethodInvokeS2(..)
  , MethodInvokeS3(..)
  , invokeS
  ) where

import Clr.Bridge
import Clr.Curry
import Clr.ListTuple
import Clr.Marshal
import Clr.Object
import Clr.Resolver
import Clr.Types

import GHC.TypeLits
import Data.Kind


--
-- Static methods
--

class MethodResultS1 (t::Type) (m::Type) (arg0::Type) where
  type ResultTypeS1 t m arg0 :: Type

class MethodResultS2 (t::Type) (m::Type) (arg0::Type) (arg1::Type) where
  type ResultTypeS2 t m arg0 arg1 :: Type

class MethodResultS3 (t::Type) (m::Type) (arg0::Type) (arg1::Type) (arg2::Type) where
  type ResultTypeS3 t m arg0 arg1 arg2 :: Type

class MethodResultS1 (t::Type) (m::Type) (arg0::Type)
   => MethodInvokeS1 (t::Type) (m::Type) (arg0::Type) where
  rawInvokeS1 :: (BridgeType arg0) -> (IO (BridgeType (ResultTypeS1 t m arg0)))

class MethodResultS2 (t::Type) (m::Type) (arg0::Type) (arg1::Type)
   => MethodInvokeS2 (t::Type) (m::Type) (arg0::Type) (arg1::Type) where
  rawInvokeS2 :: (BridgeType arg0) -> (BridgeType arg1) -> (IO (BridgeType (ResultTypeS2 t m arg0 arg1)))

class MethodResultS3 (t::Type) (m::Type) (arg0::Type) (arg1::Type) (arg2::Type)
   => MethodInvokeS3 (t::Type) (m::Type) (arg0::Type) (arg1::Type) (arg2::Type) where
  rawInvokeS3 :: (BridgeType arg0) -> (BridgeType arg1) -> (BridgeType arg2) -> (IO (BridgeType (ResultTypeS3 t m arg0 arg1 arg2)))

--
-- Unification of static methods
--

class MethodS (n::Nat) (t::Type) (m::Type) (args::[Type]) where
  type ResultTypeS n t m args :: Type
  rawInvokeS :: CurryT' n (BridgeTypes args) (IO (BridgeType (ResultTypeS n t m args)))

instance (MethodInvokeS1 t m ()) => MethodS 1 t m '[] where
  type ResultTypeS 1 t m '[] = ResultTypeS1 t m ()
  rawInvokeS = rawInvokeS1 @t @m @()

instance (MethodInvokeS1 t m a) => MethodS 1 t m '[a] where
  type ResultTypeS 1 t m '[a] = ResultTypeS1 t m a
  rawInvokeS = rawInvokeS1 @t @m @a

instance (MethodInvokeS2 t m a0 a1) => MethodS 2 t m '[a0, a1] where
  type ResultTypeS 2 t m '[a0, a1] = ResultTypeS2 t m a0 a1
  rawInvokeS = rawInvokeS2 @t @m @a0 @a1

instance (MethodInvokeS3 t m a0 a1 a2) => MethodS 3 t m '[a0, a1, a2] where
  type ResultTypeS 3 t m '[a0, a1, a2] = ResultTypeS3 t m a0 a1 a2
  rawInvokeS = rawInvokeS3 @t @m @a0 @a1 @a2

--
-- API
--

invokeS :: forall ms ts resultBridge resultHask m t argsClrUnResolved argsClr argsHask argCount argsBridge .
            ( MakeT ms ~ m
            , MakeT ts ~ t
            , ArgCount argsHask ~ argCount
            , HaskToClrL (TupleToList argsHask) ~ argsClrUnResolved
            , ResolveMember argsClrUnResolved (Candidates t m) ~ argsClr
            , MethodS argCount t m argsClr
            , ListToTuple (BridgeTypeL argsClr) ~ argsBridge
            , BridgeType (ResultTypeS argCount t m argsClr) ~ resultBridge
            , Marshal argsHask argsBridge
            , Unmarshal resultBridge resultHask
            , Curry argCount (argsBridge -> IO resultBridge) (CurryT' argCount argsBridge (IO resultBridge))
            ) => argsHask -> IO resultHask
invokeS x = marshal @argsHask @argsBridge @resultBridge x (\tup-> uncurryN @argCount (rawInvokeS @argCount @t @m @argsClr) tup) >>= unmarshal


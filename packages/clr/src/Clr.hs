{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr
  ( invokeS
  , invokeI
  , new
  , getPropI
  , setPropI
  , getPropS
  , setPropS
  , Candidates
  , module Clr.Constructor
  , module Clr.Inheritance
  , module Clr.ListTuple
  , module Clr.Method.Instance
  , module Clr.Method.Static
  , module Clr.Object
  , module Clr.Property
  , module Clr.Types
  ) where

import Clr.Bridge
import Clr.Constructor
import Clr.Curry
import Clr.Inheritance
import Clr.ListTuple
import Clr.Marshal
import Clr.Method.Instance
import Clr.Method.Static
import Clr.Object
import Clr.Property
import Clr.Resolver
import Clr.Types

import Data.Int
import Data.Kind
import Data.Type.Bool
import GHC.TypeLits

--
-- Static method invocation
--
invokeS :: forall ms ts m t argsClrUnResolved argsClr argsHask argCount argsBridge resultBridge resultHask .
            ( MakeT ms ~ m
            , MakeT ts ~ t
            , TupleSize argsHask ~ argCount
            , HaskToClrL (TupleToList argsHask) ~ argsClrUnResolved
            , ResolveMember argsClrUnResolved (Candidates t m) ~ argsClr
            , MethodS argCount t m argsClr
            , ListToTuple (BridgeTypeL argsClr) ~ argsBridge
            , BridgeTypeM (ResultTypeS argCount t m argsClr) ~ resultBridge
            , Marshal argsHask argsBridge
            , UnmarshalAs resultBridge ~ resultHask
            , Unmarshal resultBridge resultHask
            , Curry argCount (argsBridge -> IO resultBridge) (CurryT' argCount argsBridge (IO resultBridge))
            ) => argsHask -> IO resultHask
invokeS x = marshal @argsHask @argsBridge @resultBridge x (\tup-> uncurryN @argCount (rawInvokeS @argCount @t @m @argsClr) tup) >>= unmarshal

--
-- Instance method invocation
--
invokeI :: forall ms m tBase tDerived argsClrUnResolved argsClr argsHask argCount argsBridge resultBridge resultHask .
            ( MakeT ms ~ m
            , TupleSize argsHask ~ argCount
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

--
-- Constructor invocation
--
new :: forall ts t argsClrUnResolved argsClr argsHask argCount argsBridge resultBridge .
        ( MakeT ts ~ t
        , TupleSize argsHask ~ argCount
        , HaskToClrL (TupleToList argsHask) ~ argsClrUnResolved
        , ResolveMember argsClrUnResolved (Candidates t t) ~ argsClr
        , Constructor argCount t argsClr
        , ListToTuple (BridgeTypeL argsClr) ~ argsBridge
        , Marshal argsHask argsBridge
        , Unmarshal (BridgeType t) (Object t)
        , Curry argCount (argsBridge -> (IO (BridgeType t))) (CurryT' argCount argsBridge (IO (BridgeType t)))
        ) => argsHask -> IO (Object t)
new x = marshal @argsHask @argsBridge @(BridgeType t) x (\tup-> uncurryN @argCount (rawNew @argCount @t @argsClr) tup) >>= unmarshal

--
-- Instance properties
--
getPropI :: forall ms m tBase tDerived propertyBridge propertyHask .
            ( MakeT ms ~ m
            , ResolveBaseType tDerived m ~ tBase
            , tDerived `Implements` tBase ~ 'True
            , PropertyI tBase m
            , PropertyGetI tBase m
            , BridgeType (PropertyTypeI tBase m) ~ propertyBridge
            , Marshal (Object tBase) (BridgeType tBase)
            , UnmarshalAs propertyBridge ~ propertyHask
            , Unmarshal propertyBridge propertyHask
            ) => Object tDerived -> IO propertyHask
getPropI obj = marshal @(Object tBase) @(BridgeType tBase) @propertyBridge (upCast obj) (\obj'-> (rawGetPropI @tBase @m obj')) >>= unmarshal

setPropI :: forall ms m tBase tDerived propertyBridge propertyHask .
            ( MakeT ms ~ m
            , ResolveBaseType tDerived m ~ tBase
            , tDerived `Implements` tBase ~ 'True
            , PropertyI tBase m
            , PropertySetI tBase m
            , BridgeType (PropertyTypeI tBase m) ~ propertyBridge
            , Marshal (Object tBase) (BridgeType tBase)
            , Marshal propertyHask propertyBridge
            ) => Object tDerived -> propertyHask -> IO ()
setPropI obj x = marshal @(Object tBase) @(BridgeType tBase) @() (upCast obj) (\obj'-> marshal @propertyHask @propertyBridge @() x (\prop-> rawSetPropI @tBase @m obj' prop))

--
-- Static properties
--
getPropS :: forall ms m t propertyBridge propertyHask .
            ( MakeT ms ~ m
            , PropertyS t m
            , PropertyGetS t m
            , BridgeType (PropertyTypeS t m) ~ propertyBridge
            , UnmarshalAs propertyBridge ~ propertyHask
            , Unmarshal propertyBridge propertyHask
            ) => IO propertyHask
getPropS = rawGetPropS @t @m >>= unmarshal

setPropS :: forall ms m t propertyBridge propertyHask .
            ( MakeT ms ~ m
            , PropertyS t m
            , PropertySetS t m
            , BridgeType (PropertyTypeS t m) ~ propertyBridge
            , Marshal propertyHask propertyBridge
            ) => propertyHask -> IO ()
setPropS x = marshal @propertyHask @propertyBridge @() x (\prop-> rawSetPropS @t @m prop)

{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr.Property where

import Clr.Bridge
import Clr.Curry
import Clr.Inheritance
import Clr.ListTuple
import Clr.Marshal
import Clr.Object
import Clr.Resolver
import Clr.Types

import GHC.TypeLits
import Data.Kind

--
-- Unused at the moment
--

data PropertyAccessEnum = PropRead | PropWrite | PropReadWrite

type family PropertyAccess (t::Type) (m::Type) :: PropertyAccessEnum


--
-- Instance properties
--

class PropertyI (t::Type) (m::Type) where
  type PropertyTypeI t m :: Type

class PropertyI (t::Type) (m::Type) => PropertyGetI (t::Type) (m::Type) where
  rawGetPropI :: BridgeType t -> IO (BridgeType (PropertyTypeI t m))

class PropertyI (t::Type) (m::Type) => PropertySetI (t::Type) (m::Type) where
  rawSetPropI :: BridgeType t -> BridgeType (PropertyTypeI t m) -> IO ()

--
-- Static properties
--

class PropertyS (t::Type) (m::Type) where
  type PropertyTypeS t m :: Type

class PropertyS (t::Type) (m::Type) => PropertyGetS (t::Type) (m::Type) where
  rawGetPropS :: IO (BridgeType (PropertyTypeS t m))

class PropertyS (t::Type) (m::Type) => PropertySetS (t::Type) (m::Type) where
  rawSetPropS :: BridgeType (PropertyTypeS t m) -> IO ()

--
-- API
--

--
-- Instance properties
--
getPropI :: forall ms propertyBridge propertyHask m tBase tDerived .
            ( MakeT ms ~ m
            , ResolveBaseType tDerived m ~ tBase
            , tDerived `Implements` tBase ~ 'True
            , PropertyI tBase m
            , PropertyGetI tBase m
            , BridgeType (PropertyTypeI tBase m) ~ propertyBridge
            , Marshal (Object tBase) (BridgeType tBase)
            , Unmarshal propertyBridge propertyHask
            ) => Object tDerived -> IO propertyHask
getPropI obj = marshal @(Object tBase) @(BridgeType tBase) @propertyBridge (upCast obj) (\obj'-> (rawGetPropI @tBase @m obj')) >>= unmarshal

setPropI :: forall ms propertyBridge propertyHask m tBase tDerived .
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
getPropS :: forall ms ts propertyBridge propertyHask m t .
            ( MakeT ms ~ m
            , MakeT ts ~ t
            , PropertyS t m
            , PropertyGetS t m
            , BridgeType (PropertyTypeS t m) ~ propertyBridge
            , Unmarshal propertyBridge propertyHask
            ) => IO propertyHask
getPropS = rawGetPropS @t @m >>= unmarshal

setPropS :: forall ms ts propertyBridge propertyHask m t .
            ( MakeT ms ~ m
            , MakeT ts ~ t
            , PropertyS t m
            , PropertySetS t m
            , BridgeType (PropertyTypeS t m) ~ propertyBridge
            , Marshal propertyHask propertyBridge
            ) => propertyHask -> IO ()
setPropS x = marshal @propertyHask @propertyBridge @() x (\prop-> rawSetPropS @t @m prop)






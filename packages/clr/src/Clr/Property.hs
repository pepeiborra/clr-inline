{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr.Property where

import Clr.Bridge
import Clr.Curry

import GHC.TypeLits
import Data.Kind

data PropertyAccessEnum = PropRead | PropWrite | PropReadWrite

type family PropertyAccess (t::Type) (m::Type) :: PropertyAccessEnum


--
-- Instance properties
--

class PropertyI (t::Type) (m::Type) where
  type PropertyTypeI t m :: Type

class PropertyI (t::Type) (m::Type) => PropertyGetI (t::Type) (m::Type) where
  rawGetPropI :: (BridgeType t) -> IO (BridgeType (PropertyTypeI t m ))

class PropertyI (t::Type) (m::Type) => PropertySetI (t::Type) (m::Type) where
  rawSetPropI :: (BridgeType t) -> BridgeType (PropertyTypeI t m) -> IO ()

--
-- Static properties
--

class PropertyS (t::Type) (m::Type) where
  type PropertyTypeS t m :: Type

class PropertyS (t::Type) (m::Type) => PropertyGetS (t::Type) (m::Type) where
  rawGetPropS :: IO (BridgeType (PropertyTypeS t m))

class PropertyS (t::Type) (m::Type) => PropertySetS (t::Type) (m::Type) where
  rawSetPropS :: BridgeType (PropertyTypeS t m) -> IO ()



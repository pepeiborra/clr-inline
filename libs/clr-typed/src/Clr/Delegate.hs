{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr.Delegate where

import Clr.Bridge
import Clr.Curry
import Clr.ListTuple
import Clr.Marshal
import Clr.MarshalF
import Clr.Object
import Clr.Resolver
import Clr.Types

import GHC.TypeLits
import Data.Kind

class Delegate t where
  type DelegateArgTypes   t :: [Type]
  type DelegateResultType t :: Type

type family DelegateArgN (t::Type) (n::Nat) :: Type where
  DelegateArgN t n = (DelegateArgTypes t) `Index` n

class Delegate t => DelegateConstructor0 (t::Type) where
  rawConstructDelegate0 :: (IO (BridgeType (DelegateResultType t))) -> (IO (BridgeType t))

class Delegate t => DelegateConstructor1 (t::Type) where
  rawConstructDelegate1 :: (BridgeType (DelegateArgN t 0)  -> IO (BridgeType (DelegateResultType t))) -> (IO (BridgeType t))

class Delegate t => DelegateConstructor2 (t::Type) where
  rawConstructDelegate2 :: (BridgeType (DelegateArgN t 0)  -> BridgeType (DelegateArgN t 1) -> IO (BridgeType (DelegateResultType t))) -> (IO (BridgeType t))

class Delegate t => DelegateConstructor3 (t::Type) where
  rawConstructDelegate3 :: (BridgeType (DelegateArgN t 0)  -> BridgeType (DelegateArgN t 1) -> BridgeType (DelegateArgN t 2) -> IO (BridgeType (DelegateResultType t))) -> (IO (BridgeType t))

type family DelegateArity (t::Type) :: Nat where
  DelegateArity t = ListSize (DelegateArgTypes t)

type family DelegateBridgeType (t::Type) :: Type where
  DelegateBridgeType t = CurryT' (DelegateArity t) (BridgeTypes (DelegateArgTypes t)) (IO (BridgeType (DelegateResultType t)))

class Delegate t => DelegateConstructorN (n::Nat) (t::Type) where
  rawConstructDelegate :: DelegateBridgeType t -> IO (BridgeType t)

instance ( DelegateArity t ~ 0
         , DelegateConstructor0 t
         ) => DelegateConstructorN 0 t where
  rawConstructDelegate = rawConstructDelegate0 @t

instance ( DelegateArity t ~ 1
         , DelegateArgTypes t ~ '[a0]
         , DelegateConstructor1 t
         ) => DelegateConstructorN 1 t where
  rawConstructDelegate = rawConstructDelegate1 @t

instance ( DelegateArity t ~ 2
         , DelegateArgTypes t ~ '[a0, a1]
         , DelegateConstructor2 t
         ) => DelegateConstructorN 2 t where
  rawConstructDelegate = rawConstructDelegate2 @t

instance ( DelegateArity t ~ 3
         , DelegateArgTypes t ~ '[a0, a1, a2]
         , DelegateConstructor3 t
         ) => DelegateConstructorN 3 t where
  rawConstructDelegate = rawConstructDelegate3 @t

--
-- API
--

delegate :: forall ds d ht bt n .
            ( MakeT ds ~ d
            , Delegate d
            , DelegateBridgeType d ~ bt
            , DelegateArity d ~ n
            , MarshalF n ht bt
            , DelegateConstructorN n d
            , Unmarshal (BridgeType d) (Object d)
            ) => ht -> IO (Object d)
delegate f = rawConstructDelegate @n @d (marshalF @n @ht @bt f) >>= unmarshal


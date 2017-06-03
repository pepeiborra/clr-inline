{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr.Constructor
  ( Constructor(..)
  , Constructor1(..)
  , Constructor2(..)
  , Constructor3(..)
  , new
  ) where

import Clr.Bridge
import Clr.Curry
import Clr.ListTuple
import Clr.Marshal
import Clr.Object
import Clr.Resolver
import Clr.Types
import Clr.UnmarshalAs

import GHC.TypeLits
import Data.Kind

--
-- Constructors
--
class Constructor1 (t::Type) (arg0::Type) where
  rawNew1 :: BridgeType arg0 -> IO (BridgeType t)

class Constructor2 (t::Type) (arg0::Type) (arg1::Type) where
  rawNew2 :: BridgeType arg0 -> BridgeType arg1 -> IO (BridgeType t)

class Constructor3 (t::Type) (arg0::Type) (arg1::Type) (arg3::Type) where
  rawNew3 :: BridgeType arg0 -> BridgeType arg1 -> BridgeType arg3 -> IO (BridgeType t)

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

--
-- API
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


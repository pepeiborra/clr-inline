{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}


module Clr.Bindings.IEnumerable where

import Clr
import Clr.Bridge
import Clr.Marshal

import Clr.Host
import Clr.Host.BStr

import Clr.Bindings.Host

import Data.Kind
import Data.Type.Bool
import Data.Type.Equality
import Foreign.Ptr
import GHC.TypeLits

import Pipes

type T_IEnumerable t = T "System.Collections.Generic.IEnumerable" '[t]
type T_IEnumerator t = T "System.Collections.Generic.IEnumerator" '[t]

type instance SuperTypes (T_IEnumerable t) = '[ T "System.Object" '[] ]

instance MethodI1 (T_IEnumerable t) (T "GetEnumerator" '[]) () where
  type ResultTypeI1 (T_IEnumerable t) (T "GetEnumerator" '[]) () = 'Just (T_IEnumerator t)
  -- TODO: System.String is currently hardcoded below
  rawInvokeI1 ienumerable () = getMethodStub "System.Collections.Generic.IEnumerable`1[System.String]" "GetEnumerator" "" >>= return . makeGetEnumerator >>= \f-> f ienumerable

foreign import ccall "dynamic" makeGetEnumerator :: FunPtr (ObjectID a -> IO (ObjectID b)) -> (ObjectID a -> IO (ObjectID b))

type instance Members (T_IEnumerable t) = '[ T "GetEnumerator" '[] ]

type instance Candidates (T_IEnumerable t) (T "GetEnumerator" '[]) = '[ '[] ]

getEnumerator :: forall t elem . ((t `Implements` (T_IEnumerable elem)) ~ 'True, IEnumElemT t ~ elem) => Object t -> IO (Object (T_IEnumerator elem))
getEnumerator x = getEnumerator' $ upCast x
  where getEnumerator' :: Object (T_IEnumerable elem) -> IO (Object (T_IEnumerator elem))
        getEnumerator' x = invokeI @"GetEnumerator" x ()

type family IEnumElemT (x::Type) :: Type where
  IEnumElemT t = IEnumElemT' (WithAllSuperTypes t)

type family IEnumElemT' (x::[Type]) :: Type where
  IEnumElemT'        '[]                                                   = TypeError (Text "Not an IEnumerable")
  IEnumElemT' ((T "System.Collections.Generic.IEnumerable" '[elem]) ': xs) = elem
  IEnumElemT'     (x ': xs)                                                = IEnumElemT' xs

type T_Current = T "Current" '[]

instance PropertyI (T_IEnumerator t) T_Current where
  type PropertyTypeI (T_IEnumerator t) T_Current = t

instance PropertyGetI (T_IEnumerator (T "System.String" '[])) T_Current where
  -- TODO: System.String is currently hardcoded above & below
  rawGetPropI ienumerator = getMethodStub "System.Collections.Generic.IEnumerator`1[System.String]" "get_Current" "" >>= return . makeEnumeratorCurrent >>= \f-> f ienumerator

type instance Members (T_IEnumerator t) = '[ T_Current, T_MoveNext ]

foreign import ccall "dynamic" makeEnumeratorCurrent :: FunPtr (ObjectID (T_IEnumerator elem) -> IO BStr) -> (ObjectID (T_IEnumerator elem) -> IO BStr)

ienumCurrent :: forall elem propertyBridge propertyHask .
  ( PropertyGetI (T_IEnumerator elem) T_Current
  , BridgeType (PropertyTypeI (T_IEnumerator elem) T_Current) ~ propertyBridge
  , UnmarshalAs propertyBridge ~ propertyHask
  , Unmarshal propertyBridge propertyHask
  ) => Object (T_IEnumerator elem) -> IO propertyHask
ienumCurrent ienum = getPropI @T_Current ienum

type T_MoveNext = T "MoveNext" '[]

type instance Candidates (T_IEnumerator t) T_MoveNext = '[ '[] ]

instance MethodI1 (T_IEnumerator t) T_MoveNext () where
  type ResultTypeI1 (T_IEnumerator t) T_MoveNext () = 'Just (T "System.Boolean" '[])
  rawInvokeI1 ienumerator () = getMethodStub "System.Collections.IEnumerator" "MoveNext" "" >>= return . makeEnumeratorMoveNext >>= \f-> f ienumerator

foreign import ccall "dynamic" makeEnumeratorMoveNext :: FunPtr (ObjectID (T_IEnumerator elem) -> IO Bool) -> (ObjectID (T_IEnumerator elem) -> IO Bool) 

ienumMoveNext :: forall elem . Object (T_IEnumerator elem) -> IO Bool
ienumMoveNext ienum = invokeI @T_MoveNext ienum ()

toProducer :: forall t elem elemBridge elemHask .
  ( (t `Implements` (T_IEnumerable elem)) ~ 'True
  , IEnumElemT t ~ elem
  , PropertyGetI (T_IEnumerator elem) T_Current
  , BridgeType (PropertyTypeI (T_IEnumerator elem) T_Current) ~ elemBridge
  , UnmarshalAs elemBridge ~ elemHask
  , Unmarshal elemBridge elemHask
  ) => Object t -> Producer elemHask IO ()
toProducer ienumerable = do
  ienumerator <- liftIO $ getEnumerator ienumerable
  toProducer' ienumerator

toProducer' :: forall elem elemBridge elemHask .
  ( PropertyGetI (T_IEnumerator elem) T_Current
  , BridgeType (PropertyTypeI (T_IEnumerator elem) T_Current) ~ elemBridge
  , UnmarshalAs elemBridge ~ elemHask
  , Unmarshal elemBridge elemHask
  ) => Object (T_IEnumerator elem) -> Producer elemHask IO ()
toProducer' ienumerator = do
  nxt <- liftIO $ ienumMoveNext ienumerator
  if nxt then do
    cur <- liftIO $ ienumCurrent ienumerator
    yield cur
    toProducer' ienumerator
  else return ()


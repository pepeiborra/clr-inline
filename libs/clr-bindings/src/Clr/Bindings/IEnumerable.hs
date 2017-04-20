{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}


module Clr.Bindings.IEnumerable where

import Clr
import Clr.Bridge
import Clr.Marshal
import Clr.UnmarshalAs
import Clr.TypeString

import Clr.Host
import Clr.Host.BStr

import Clr.Bindings.DynImports
import Clr.Bindings.Host

import Data.Kind
import Data.Type.Bool
import Data.Type.Equality
import Foreign.Ptr
import GHC.TypeLits

import Pipes
import qualified Pipes.Prelude

type T_IEnumerable t = T "System.Collections.Generic.IEnumerable" '[t]
type T_IEnumerable'  = T "System.Collections.IEnumerable" '[]
type T_IEnumerator t = T "System.Collections.Generic.IEnumerator" '[t]
type T_IEnumerator'  = T "System.Collections.IEnumerator" '[]

type T_Current       = T "Current" '[]
type T_MoveNext      = T "MoveNext" '[]
type T_GetEnumerator = T "GetEnumerator" '[]

type instance Members (T_IEnumerable t) = '[ T_GetEnumerator ]
type instance Members (T_IEnumerator t) = '[ T_Current ]
type instance Members  T_IEnumerator'   = '[ T_Current, T_MoveNext ]

type instance Candidates (T_IEnumerable t) T_GetEnumerator = '[ '[] ]
type instance Candidates  T_IEnumerator'   T_MoveNext      = '[ '[] ]

type instance SuperTypes (T_IEnumerable t) = '[ T_IEnumerable', T_object ]
type instance SuperTypes (T_IEnumerator t) = '[ T_IEnumerator', T_object ]

foreign import ccall "dynamic" makeGetEnumerator      :: FunPtr (ObjectID a -> IO (ObjectID b)) -> (ObjectID a -> IO (ObjectID b))
foreign import ccall "dynamic" makeEnumeratorMoveNext :: FunPtr (ObjectID (T_IEnumerator') -> IO Bool) -> (ObjectID (T_IEnumerator') -> IO Bool)

foreign import ccall "dynamic" makeEnumeratorCurrentBStr  :: FunPtr (ObjectID (T_IEnumerator elem) -> IO BStr) -> (ObjectID (T_IEnumerator elem) -> IO BStr)
foreign import ccall "dynamic" makeEnumeratorCurrentBool  :: FunPtr (ObjectID (T_IEnumerator elem) -> IO Bool) -> (ObjectID (T_IEnumerator elem) -> IO Bool)
-- TODO: makeEnumCurrent_ for every other prim type
foreign import ccall "dynamic" makeEnumeratorCurrentObj   :: FunPtr (ObjectID (T_IEnumerator elem) -> IO (ObjectID elem)) -> (ObjectID (T_IEnumerator elem) -> IO (ObjectID elem))

instance MethodResultI1 (T_IEnumerable t) (T_GetEnumerator) () where
  type ResultTypeI1 (T_IEnumerable t) (T_GetEnumerator) () = 'Just (T_IEnumerator t)

instance MethodDynImportI1 (T_IEnumerable t) (T_GetEnumerator) () where
  methodDynImportI1 = makeGetEnumerator

instance MethodResultI1 T_IEnumerator' T_MoveNext () where
  type ResultTypeI1 T_IEnumerator' T_MoveNext () = 'Just T_bool

instance MethodDynImportI1 T_IEnumerator' T_MoveNext () where
  methodDynImportI1 = makeEnumeratorMoveNext

instance PropertyI (T_IEnumerator t) T_Current where
  type PropertyTypeI (T_IEnumerator t) T_Current = t

instance {-# OVERLAPS #-} PropertyDynImportGetI (T_IEnumerator T_string) T_Current where
  propertyDynImportGetI = makeEnumeratorCurrentBStr

instance {-# OVERLAPS #-} PropertyDynImportGetI (T_IEnumerator T_bool) T_Current where
  propertyDynImportGetI = makeEnumeratorCurrentBool
--
-- TODO: PropertyDynImportGetI for every other prim type
--
instance {-# OVERLAPS #-} (IsPrimType (T name gt) ~ 'False) => PropertyDynImportGetI (T_IEnumerator (T name gt)) T_Current where
  propertyDynImportGetI = makeEnumeratorCurrentObj


type family IEnumElemT (x::Type) :: Type where
  IEnumElemT t = IEnumElemT' (WithAllSuperTypes t)

type family IEnumElemT' (x::[Type]) :: Type where
  IEnumElemT'        '[]                                                   = TypeError (Text "Not an instance of System.Collections.Generic.IEnumerable")
  IEnumElemT' ((T "System.Collections.Generic.IEnumerable" '[elem]) ': xs) = elem
  IEnumElemT'     (x ': xs)                                                = IEnumElemT' xs

getEnumerator :: forall t elem . ((t `Implements` (T_IEnumerable elem)) ~ 'True, IEnumElemT t ~ elem, TString elem) => Object t -> IO (Object (T_IEnumerator elem))
getEnumerator x = getEnumerator' $ upCast x
  where getEnumerator' :: Object (T_IEnumerable elem) -> IO (Object (T_IEnumerator elem))
        getEnumerator' x = invokeI @T_GetEnumerator x ()

ienumCurrent :: forall elem propertyBridge propertyHask .
  ( PropertyGetI (T_IEnumerator elem) T_Current
  , BridgeType (PropertyTypeI (T_IEnumerator elem) T_Current) ~ propertyBridge
  , UnmarshalAs propertyBridge ~ propertyHask
  , Unmarshal propertyBridge propertyHask
  ) => Object (T_IEnumerator elem) -> IO propertyHask
ienumCurrent ienum = getPropI @T_Current ienum

ienumMoveNext :: forall elem . Object (T_IEnumerator elem) -> IO Bool
ienumMoveNext ienum = invokeI @T_MoveNext ienum ()

toProducer :: forall t elem elemBridge elemHask .
  ( (t `Implements` (T_IEnumerable elem)) ~ 'True
  , IEnumElemT t ~ elem
  , TString elem
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

toListM :: forall t elem elemBridge elemHask .
  ( (t `Implements` (T_IEnumerable elem)) ~ 'True
  , IEnumElemT t ~ elem
  , TString elem
  , PropertyGetI (T_IEnumerator elem) T_Current
  , BridgeType (PropertyTypeI (T_IEnumerator elem) T_Current) ~ elemBridge
  , UnmarshalAs elemBridge ~ elemHask
  , Unmarshal elemBridge elemHask
  ) => Object t -> IO [elemHask]
toListM ienumerable = Pipes.Prelude.toListM $ toProducer ienumerable


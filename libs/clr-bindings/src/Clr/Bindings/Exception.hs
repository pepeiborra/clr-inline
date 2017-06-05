{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Clr.Bindings.Exception where

import Clr
import Clr.Bridge
import Clr.Curry
import Clr.Resolver
import Clr.UnmarshalAs

import Clr.Bindings.BStr
import Clr.Bindings.DynImports
import Clr.Bindings.Object
import Clr.Bindings.WrapImports

import Clr.Host.BStr
import Clr.Host.GCHandle

import Clr.Marshal
import Clr.MarshalF

import GHC.TypeLits
import Data.Kind
import Foreign.Ptr


type T_Driver               = T "Salsa.Driver" '[]
type T_TryDelegate   a      = T "Salsa.TryDelegate" '[a]
type T_CatchDelegate a ex   = T "Salsa.CatchDelegate" '[a, ex]

type T_RunCatchHandler a ex = T "RunCatchHandler" '[a, ex]

instance MethodResultS2 T_Driver (T_RunCatchHandler a ex) tryD catchD where
  type ResultTypeS2 T_Driver (T_RunCatchHandler a ex) tryD catchD = 'Just a

instance {-# OVERLAPS #-} (IsPrimType a ~ 'False, BridgeType a ~ GCHandle a) => MethodDynImportS2 T_Driver (T_RunCatchHandler a ex) (T_TryDelegate a) (T_CatchDelegate a ex) where
  methodDynImportS2 = makeRunCatchHandlerObj

instance {-# OVERLAPS #-} MethodDynImportS2 T_Driver (T_RunCatchHandler T_string ex) (T_TryDelegate T_string) (T_CatchDelegate T_string ex) where
  methodDynImportS2 = makeRunCatchHandlerBStr

foreign import ccall "wrapper" wrapTryDelegateObj :: (IO (GCHandle a)) -> IO (FunPtr (IO (GCHandle a)))
foreign import ccall "wrapper" wrapCatchDelegateObj :: (GCHandle ex -> IO (GCHandle a)) -> IO (FunPtr (GCHandle ex -> IO (GCHandle a)))

foreign import ccall "wrapper" wrapTryDelegateBStr :: (IO BStr) -> IO (FunPtr (IO BStr))
foreign import ccall "wrapper" wrapCatchDelegateBStr :: (GCHandle ex -> IO BStr) -> IO (FunPtr (GCHandle ex -> IO BStr))

foreign import ccall "dynamic" makeRunCatchHandlerObj :: FunPtr (GCHandle tryD -> GCHandle catchD -> IO (GCHandle a)) -> (GCHandle tryD -> GCHandle catchD -> IO (GCHandle a))
foreign import ccall "dynamic" makeRunCatchHandlerBStr :: FunPtr (GCHandle tryD -> GCHandle catchD -> IO BStr) -> (GCHandle tryD -> GCHandle catchD -> IO BStr)

instance Delegate (T_TryDelegate a) where
  type DelegateArgTypes   (T_TryDelegate a)      = '[]
  type DelegateResultType (T_TryDelegate a)      = 'Just a

instance Delegate (T_CatchDelegate a ex) where
  type DelegateArgTypes   (T_CatchDelegate a ex) = '[ ex ]
  type DelegateResultType (T_CatchDelegate a ex) = 'Just a

instance {-# OVERLAPS #-} (IsPrimType a ~ 'False, BridgeType a ~ GCHandle a) => WrapperImport (T_TryDelegate a) where
  wrapperImport = wrapTryDelegateObj

instance {-# OVERLAPS #-} (IsPrimType a ~ 'False, BridgeType a ~ GCHandle a, BridgeType ex ~ GCHandle ex) => WrapperImport (T_CatchDelegate a ex) where
  wrapperImport = wrapCatchDelegateObj

instance {-# OVERLAPS #-} WrapperImport (T_TryDelegate T_string) where
  wrapperImport = wrapTryDelegateBStr

instance {-# OVERLAPS #-} (BridgeType ex ~ GCHandle ex) => WrapperImport (T_CatchDelegate T_string ex) where
  wrapperImport = wrapCatchDelegateBStr

catch :: forall dTry dCatch driver catchHandler bTry bCatch retHask retClr ex argsClrUnResolved argsClr argsBridge resultBridge .
          ( ex `Implements` (T "System.Exception" '[]) ~ 'True
          , dTry   ~ (T "Salsa.TryDelegate"   '[retClr])
          , dCatch ~ (T "Salsa.CatchDelegate" '[retClr, ex])
          , driver ~ (T "Salsa.Driver" '[])
          , catchHandler ~ (T "RunCatchHander" '[retClr, ex])
          , Delegate dTry
          , Delegate dCatch
          , DelegateArity dTry   ~ 0
          , DelegateArity dCatch ~ 1
          , DelegateBridgeType dTry   ~ bTry
          , DelegateBridgeType dCatch ~ bCatch
          , MarshalF 0 (             IO retHask) (bTry  )
          , MarshalF 1 (Object ex -> IO retHask) (bCatch)
          , DelegateConstructorN 0 dTry
          , DelegateConstructorN 1 dCatch
          , Unmarshal (BridgeType dTry  ) (Object dTry  )
          , Unmarshal (BridgeType dCatch) (Object dCatch)
          , HaskToClrL (TupleToList (IO retHask, (Object ex -> IO retHask))) ~ argsClrUnResolved
          , ResolveMember argsClrUnResolved (Candidates driver catchHandler) ~ argsClr
          , MethodS 2 driver catchHandler argsClr
          , ListToTuple (BridgeTypeL argsClr) ~ argsBridge
          , BridgeTypeM (ResultTypeS 2 driver catchHandler argsClr) ~ resultBridge
          , Marshal (IO retHask, (Object ex -> IO retHask)) argsBridge
          , UnmarshalAs resultBridge ~ retHask
          , Unmarshal resultBridge retHask
          , Curry 2 (argsBridge -> IO resultBridge) (CurryT' 2 argsBridge (IO resultBridge))
          ) => IO retHask -> (Object ex -> IO retHask) -> IO retHask
catch tryF catchF = do
  tryD   <- delegate @dTry   tryF
  catchD <- delegate @dCatch catchF
  result <- invokeS @driver @catchHandler (tryD, catchD)
  return result



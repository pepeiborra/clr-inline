{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Clr.Bindings.Exception where

import Clr
import Clr.Bridge
import Clr.Curry
import Clr.Resolver
import Clr.TypeString
import Clr.Types

import Clr.Bindings.BStr
import Clr.Bindings.DynImports
import Clr.Bindings.Object
import Clr.Bindings.WrapImports

import Clr.Host.BStr
import Clr.Host.GCHandle

import Clr.Marshal
import Clr.MarshalF

import Control.Concurrent
import GHC.TypeLits
import qualified Control.Exception as Ex
import Data.Kind
import Data.Typeable
import Foreign.Ptr

import Data.Text


type T_Driver               = T "Salsa.Driver" '[]
type T_TryDelegate   a      = T "Salsa.TryDelegate" '[a]
type T_CatchDelegate a ex   = T "Salsa.CatchDelegate" '[a, ex]

type T_RunCatchHandler a ex = T "RunCatchHandler" '[a, ex]

type instance Candidates T_Driver (T_RunCatchHandler a ex) = '[ '[ T_TryDelegate a, T_CatchDelegate a ex] ]

type instance SuperTypes (T_TryDelegate a) = '[ T_object ]
type instance SuperTypes (T_CatchDelegate a ex) = '[ T_object ]

instance MethodResultS2 T_Driver (T_RunCatchHandler a ex) (T_TryDelegate a) (T_CatchDelegate a ex) where
  type ResultTypeS2 T_Driver (T_RunCatchHandler a ex) (T_TryDelegate a) (T_CatchDelegate a ex) = a

instance {-# OVERLAPS #-} (IsPrimType a ~ 'False, BridgeType a ~ GCHandle a) => MethodDynImportS2 T_Driver (T_RunCatchHandler a ex) (T_TryDelegate a) (T_CatchDelegate a ex) where
  methodDynImportS2 = makeRunCatchHandlerObj

instance {-# OVERLAPS #-} MethodDynImportS2 T_Driver (T_RunCatchHandler T_string ex) (T_TryDelegate T_string) (T_CatchDelegate T_string ex) where
  methodDynImportS2 = makeRunCatchHandlerBStr

foreign import ccall "wrapper" wrapTryDelegateObj :: (IO (GCHandle a)) -> IO (FunPtr (IO (GCHandle a)))
foreign import ccall "wrapper" wrapCatchDelegateObj :: (GCHandle ex -> IO (GCHandle a)) -> IO (FunPtr (GCHandle ex -> IO (GCHandle a)))

foreign import ccall "wrapper" wrapTryDelegateBStr :: (IO BStr) -> IO (FunPtr (IO BStr))
foreign import ccall "wrapper" wrapCatchDelegateBStr :: (GCHandle ex -> IO BStr) -> IO (FunPtr (GCHandle ex -> IO BStr))

foreign import ccall "dynamic"  makeRunCatchHandlerObj :: FunPtr (GCHandle tryD -> GCHandle catchD -> IO (GCHandle a)) -> (GCHandle tryD -> GCHandle catchD -> IO (GCHandle a))
foreign import ccall "dynamic" makeRunCatchHandlerBStr :: FunPtr (GCHandle tryD -> GCHandle catchD -> IO BStr) -> (GCHandle tryD -> GCHandle catchD -> IO BStr)

instance Delegate (T_TryDelegate a) where
  type DelegateArgTypes   (T_TryDelegate a)      = '[]
  type DelegateResultType (T_TryDelegate a)      = a

instance Delegate (T_CatchDelegate a ex) where
  type DelegateArgTypes   (T_CatchDelegate a ex) = '[ ex ]
  type DelegateResultType (T_CatchDelegate a ex) = a

instance {-# OVERLAPS #-} (IsPrimType a ~ 'False, BridgeType a ~ GCHandle a) => WrapperImport (T_TryDelegate a) where
  wrapperImport = wrapTryDelegateObj

instance {-# OVERLAPS #-} (IsPrimType a ~ 'False, BridgeType a ~ GCHandle a, BridgeType ex ~ GCHandle ex) => WrapperImport (T_CatchDelegate a ex) where
  wrapperImport = wrapCatchDelegateObj

instance {-# OVERLAPS #-} WrapperImport (T_TryDelegate T_string) where
  wrapperImport = wrapTryDelegateBStr

instance {-# OVERLAPS #-} (BridgeType ex ~ GCHandle ex) => WrapperImport (T_CatchDelegate T_string ex) where
  wrapperImport = wrapCatchDelegateBStr

data CatchResult (a::Type) = CatchResult {getResult :: a}
    deriving (Show, Typeable)

instance (Show a, Typeable a) => Ex.Exception (CatchResult a)

catch :: --forall haskArgs haskResult haskTry haskCatch
           --     t_dTry t_dCatch t_handler t_ex t_args t_result
             --   b_try b_catch b_args b_result .
          ( _
          ) => IO haskResult -> (Object t_ex -> IO haskResult) -> IO haskResult
catch tryF catchF = do
  tryD   <- makeTryDelegate tryF
  catchD <- makeCatchDelegate catchF
  runCatchHandler (tryD, catchD)

makeTryDelegate :: forall t_dTry b_try t_result haskResult .
                    ( t_dTry ~ (T_TryDelegate t_result)
                    , Delegate t_dTry
                    , DelegateArity t_dTry ~ 0
                    , DelegateConstructorN 0 t_dTry
                    , DelegateBridgeType t_dTry ~ b_try
                    , DelegateResultType t_dTry ~ t_result
                    , MarshalF 0 (IO haskResult) b_try
                    , TString t_result
                    ) => IO haskResult -> IO (Object (T_TryDelegate t_result))
makeTryDelegate f = delegate @t_dTry f

makeCatchDelegate :: forall t_dCatch b_catch t_result haskResult t_ex .
                    ( t_dCatch ~ (T_CatchDelegate t_result t_ex)
                    , Delegate t_dCatch
                    , DelegateArity t_dCatch ~ 1
                    , DelegateConstructorN 1 t_dCatch
                    , DelegateBridgeType t_dCatch ~ b_catch
                    , DelegateResultType t_dCatch ~ t_result
                    , MarshalF 1 (Object t_ex -> IO haskResult) b_catch
                    , TString t_result
                    , TString t_ex
                    , t_ex `Implements` (T "System.Exception" '[]) ~ 'True
                    , IsPrimType t_ex ~ 'False
                    , BridgeType t_ex ~ GCHandle t_ex
                    , t_result ~ HaskToClr haskResult
                    , Unmarshal haskResult (BridgeType t_result)
                    ) => (Object t_ex -> IO haskResult) -> IO (Object (T_CatchDelegate t_result t_ex))
makeCatchDelegate f = delegate @t_dCatch f


runCatchHandler :: forall b_result haskResult t_result t_ex t_args t_dTry t_dCatch t_handler b_args .
                    ( t_ex `Implements` (T "System.Exception" '[]) ~ 'True
                    , t_dTry    ~ (T "Salsa.TryDelegate"   '[t_result])
                    , t_dCatch  ~ (T "Salsa.CatchDelegate" '[t_result, t_ex])
                    , t_handler ~ (T_RunCatchHandler t_result t_ex)
                    , t_args ~ '[ t_dTry, t_dCatch ]
                    , BridgeTypes t_args ~ b_args
                    , ResolveMember t_args (Candidates T_Driver t_handler) ~ t_args
                    , MethodS 2 T_Driver t_handler t_args
                    , BridgeType t_result ~ b_result
                    , Unmarshal b_result haskResult
                    ) => (Object (T_TryDelegate t_result), Object (T_CatchDelegate t_result t_ex))  -> IO haskResult
runCatchHandler (tryDel, catchDel) = invokeS @t_handler @T_Driver @b_result @haskResult (tryDel, catchDel)

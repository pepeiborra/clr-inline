{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, UndecidableInstances #-}

module Clr.Bindings.WrapImports where

import Clr.Object
import Clr.Bridge
import Clr.Delegate
import Clr.TypeString

import Clr.Host
import Clr.Host.BStr

import Clr.Marshal

import Data.Kind
import Foreign.Ptr

-- TODO mve this to host with ObjectID cleanup

-- | @'getDelegateConstructorStub' dt wrapper@ returns an action that, given a
--   function, will return a reference to a .NET delegate object that calls the
--   provided function.  The delegate constructed will be of the type @dt@.
--   The function @wrapper@ will be called in order to wrap the given function
--   as a function pointer for passing into .NET.
getDelegateConstructorStub :: String -> (f -> IO (FunPtr f)) -> IO (f -> IO (ObjectID t))
getDelegateConstructorStub delegateTypeName wrapper = do
  -- Obtain a function pointer to a function that, when called with a
  -- function pointer compatible with the given wrapper function, returns
  -- a reference to a .NET delegate object that calls the function.
  delegateConstructor <- marshal delegateTypeName $
    \delegateTypeName' -> getDelegateConstructorStubRaw >>= \x-> x delegateTypeName'

  -- Returns a function that accepts a function, 'f' implementing the
  -- delegate, converts 'f' to a function pointer, and then wraps it
  -- up as a .NET delegate.
  return $ \f -> do
    fFunPtr <- wrapper f
    (makeDelegateConstructor delegateConstructor) fFunPtr

getDelegateConstructorStubRaw :: IO (GetDelegateConstructorStubDelegate f t)
getDelegateConstructorStubRaw =  unsafeGetPointerToMethod "GetDelegateConstructorStub" >>= return . makeGetDelegateConstructorStubDelegate

type GetDelegateConstructorStubDelegate f t = BStr -> IO (FunPtr (FunPtr f -> IO (ObjectID t)))
foreign import ccall "dynamic" makeGetDelegateConstructorStubDelegate :: FunPtr (GetDelegateConstructorStubDelegate f t) -> (GetDelegateConstructorStubDelegate f t)

type DelegateConstructor f t = FunPtr f -> IO (ObjectID t)
foreign import ccall "dynamic" makeDelegateConstructor :: FunPtr (DelegateConstructor f t) -> (DelegateConstructor f t)

--
-- WrapperImportType t is the type of a function from a `t` to a `FunPtr t`
--
type family WrapperImportType t where
  WrapperImportType t = t -> IO (FunPtr t)

class WrapperImport t where
  wrapperImport :: WrapperImportType (DelegateBridgeType t)

instance ( TString t
         , Delegate t
         , BridgeType t ~ ObjectID t
         , DelegateArgTypes t ~ '[]
         , WrapperImport t
         ) => DelegateConstructor0 t where
  rawConstructDelegate0 f = do
    delCtor <- getDelegateConstructorStub (tString @t) (wrapperImport @t)
    delCtor f

instance ( TString t
         , Delegate t
         , BridgeType t ~ ObjectID t
         , DelegateArgTypes t ~ '[ a0 ]
         , WrapperImport t
         ) => DelegateConstructor1 t where
  rawConstructDelegate1 f = do
    delCtor <- getDelegateConstructorStub (tString @t) (wrapperImport @t)
    delCtor f

instance ( TString t
         , Delegate t
         , BridgeType t ~ ObjectID t
         , DelegateArgTypes t ~ '[ a0, a1 ]
         , WrapperImport t
         ) => DelegateConstructor2 t where
  rawConstructDelegate2 f = do
    delCtor <- getDelegateConstructorStub (tString @t) (wrapperImport @t)
    delCtor f



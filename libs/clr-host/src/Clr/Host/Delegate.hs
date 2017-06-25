
module Clr.Host.Delegate where

import Clr.Host.BStr
import Clr.Host.GCHandle
import Clr.Host.DriverEntryPoints

import Clr.Marshal

import Data.Kind
import Foreign.Ptr

-- | @'getDelegateConstructorStub' dt wrapper@ returns an action that, given a
--   function, will return a reference to a .NET delegate object that calls the
--   provided function.  The delegate constructed will be of the type @dt@.
--   The function @wrapper@ will be called in order to wrap the given function
--   as a function pointer for passing into .NET.
getDelegateConstructorStub :: String -> (f -> IO (FunPtr f)) -> IO (f -> IO (GCHandle t))
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

type GetDelegateConstructorStubDelegate f t = BStr -> IO (FunPtr (DelegateConstructor f t))
foreign import ccall "dynamic" makeGetDelegateConstructorStubDelegate :: FunPtr (GetDelegateConstructorStubDelegate f t) -> (GetDelegateConstructorStubDelegate f t)

type DelegateConstructor f t = FunPtr f -> IO (GCHandle t)
foreign import ccall "dynamic" makeDelegateConstructor :: FunPtr (DelegateConstructor f t) -> (DelegateConstructor f t)


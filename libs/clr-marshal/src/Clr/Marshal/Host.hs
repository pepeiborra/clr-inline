{-# LANGUAGE ScopedTypeVariables #-}

module Clr.Marshal.Host where

import Clr.Marshal

import Clr.Host
import Clr.Host.BStr

import Data.Coerce
import Data.Word
import Foreign.Ptr

-- | @'unsafeGetPointerToMethod' m@ returns a function pointer to the method @m@
--  as implemented in the Salsa .NET driver assembly (Salsa.dll).  It is safe only
--  if the type of the resulting function pointer matches that of the method given.
unsafeGetPointerToMethod :: String -> IO (FunPtr a)
unsafeGetPointerToMethod methodName = do
  result <- marshal methodName $ \(methodName'::BStr) -> getPointerToMethodRaw >>= \f-> f $ coerce methodName'
  if result == nullFunPtr
    then error $ "Unable to execute Salsa.dll method '" ++ methodName ++ "'."
    else return result

getPointerToMethodRaw :: IO (GetPtrToMethod a)
getPointerToMethodRaw = getPtrToMethod_get >>= return . makeGetPtrToMethod

foreign import ccall "dynamic" makeGetPtrToMethod :: GetPtrToMethodFunPtr a -> GetPtrToMethod a

-- | @'getMethodStub' c m s@ returns a function pointer to a function that, when
--   called, invokes the method with name @m@ and signature @s@ in class @c@.
--
--   @s@ should be a semi-colon delimited list of parameter types indicating the
--   desired overload of the given method.
getMethodStub :: String -> String -> String -> IO (FunPtr f)
getMethodStub className methodName parameterTypeNames = do
  marshal className $ \className' ->
    marshal methodName $ \methodName' ->
      marshal parameterTypeNames $ \parameterTypeNames' ->
        getMethodStubRaw >>= \f-> return $ f className' methodName' parameterTypeNames'

getMethodStubRaw :: IO (GetMethodStubDelegate a)
getMethodStubRaw = unsafeGetPointerToMethod "GetMethodStub" >>= return . makeGetMethodStubDelegate

type GetMethodStubDelegate a = BStr -> BStr -> BStr -> FunPtr a
foreign import ccall "dynamic" makeGetMethodStubDelegate :: FunPtr (GetMethodStubDelegate a) -> GetMethodStubDelegate a



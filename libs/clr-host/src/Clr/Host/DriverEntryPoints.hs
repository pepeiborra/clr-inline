{-# LANGUAGE ScopedTypeVariables #-}

module Clr.Host.DriverEntryPoints where

import Clr.Host.BStr
import Clr.Marshal

import Data.Coerce
import Data.Word
import Foreign.Ptr

type GetPtrToMethod a       = Ptr Word16 -> IO (FunPtr a)
type GetPtrToMethodFunPtr a = FunPtr (GetPtrToMethod a)

foreign import ccall "clrHost.c getPointerToMethod_set" getPtrToMethod_set :: GetPtrToMethodFunPtr a -> IO ()
foreign import ccall "clrHost.c getPointerToMethod_get" getPtrToMethod_get :: IO (GetPtrToMethodFunPtr a)

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

-- | 'saveDynamicAssembly' saves the assembly containing the dynamically-generated
--   wrapper stubs to disk (for debugging purposes).
saveDynamicAssembly :: IO ()
saveDynamicAssembly = unsafeGetPointerToMethod "SaveDynamicAssembly" >>=  makeSaveDynamicAssemblyDelegate

type SaveDynamicAssemblyDelegate = IO ()
foreign import ccall "dynamic" makeSaveDynamicAssemblyDelegate :: FunPtr SaveDynamicAssemblyDelegate -> SaveDynamicAssemblyDelegate


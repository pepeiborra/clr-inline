{-# LANGUAGE CPP, RankNTypes #-}

module Clr.Host where

import Clr.Host.Config
#ifdef HAVE_MONO
import Clr.Host.Mono
#endif
#ifdef HAVE_DOTNET
import Clr.Host.DotNet
#endif

import Foreign.Ptr
import Foreign.C.String

type GetPtrToMethod a       = CString -> IO (FunPtr a)
type GetPtrToMethodFunPtr a = FunPtr (GetPtrToMethod a)

foreign import ccall "clrHost.c getPointerToMethod_set" getPtrToMethod_set :: GetPtrToMethodFunPtr a -> IO ()
foreign import ccall "clrHost.c getPointerToMethod_get" getPtrToMethod_get :: IO (GetPtrToMethodFunPtr a)

startClr :: IO ()
startClr = do
  putStrLn "startClr"
  ClrHostConfig hostType <- getClrHostConfig
  getPtrToMethod <- case hostType of
#ifdef HAVE_MONO
    ClrHostMono   -> startHostMono
#else
    ClrHostMono   -> error "not built with Mono support enabled"
#endif
#ifdef HAVE_DOTNET
    ClrHostDotNet -> startHostDotNet
#else
    ClrHostDotNet -> error "not built with .Net support enabled"
#endif
  if getPtrToMethod == nullFunPtr then
    error "Failed to boot the driver"
  else
    getPtrToMethod_set getPtrToMethod

stopClr :: IO ()
stopClr = putStrLn "stopClr"

-- | @'unsafeGetPointerToMethod' m@ returns a function pointer to the method @m@
--  as implemented in the Salsa .NET driver assembly (Salsa.dll).  It is safe only
--  if the type of the resulting function pointer matches that of the method given.
unsafeGetPointerToMethod :: String -> IO (FunPtr a)
unsafeGetPointerToMethod methodName = do
  result <- withCString methodName $ \methodName' -> getPointerToMethodRaw >>= \f-> f methodName'
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
  withCString className $ \className' ->
    withCString methodName $ \methodName' ->
      withCString parameterTypeNames $ \parameterTypeNames' ->
        getMethodStubRaw >>= \f-> return $ f className' methodName' parameterTypeNames'

getMethodStubRaw :: IO (GetMethodStubDelegate a)
getMethodStubRaw = unsafeGetPointerToMethod "GetMethodStub" >>= return . makeGetMethodStubDelegate

type GetMethodStubDelegate a = CString -> CString -> CString -> FunPtr a
foreign import ccall "dynamic" makeGetMethodStubDelegate :: FunPtr (GetMethodStubDelegate a) -> GetMethodStubDelegate a


-- | 'saveDynamicAssembly' saves the assembly containing the dynamically-generated
--   wrapper stubs to disk (for debugging purposes).
saveDynamicAssembly :: IO ()
saveDynamicAssembly = unsafeGetPointerToMethod "SaveDynamicAssembly" >>=  makeSaveDynamicAssemblyDelegate

type SaveDynamicAssemblyDelegate = IO ()
foreign import ccall "dynamic" makeSaveDynamicAssemblyDelegate :: FunPtr SaveDynamicAssemblyDelegate -> SaveDynamicAssemblyDelegate



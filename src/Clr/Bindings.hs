module Clr.Bindings
  ( module Clr.Bindings.Box
  , getMethodStub
  , saveDynamicAssembly
  ) where


import Clr.Bindings.Box
import Clr.Bindings.Host
import Clr.Host
import Clr.Marshal
import Data.Word
import Foreign.Ptr

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

type GetMethodStubDelegate a = Ptr Word16 -> Ptr Word16 -> Ptr Word16 -> FunPtr a
foreign import ccall "dynamic" makeGetMethodStubDelegate :: FunPtr (GetMethodStubDelegate a) -> GetMethodStubDelegate a


-- | 'saveDynamicAssembly' saves the assembly containing the dynamically-generated
--   wrapper stubs to disk (for debugging purposes).
saveDynamicAssembly :: IO ()
saveDynamicAssembly = unsafeGetPointerToMethod "SaveDynamicAssembly" >>=  makeSaveDynamicAssemblyDelegate

type SaveDynamicAssemblyDelegate = IO ()
foreign import ccall "dynamic" makeSaveDynamicAssemblyDelegate :: FunPtr SaveDynamicAssemblyDelegate -> SaveDynamicAssemblyDelegate





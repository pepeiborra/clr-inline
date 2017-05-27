
module Clr.Host.Method where

import Clr.Marshal
import Clr.Host.BStr
import Clr.Host.DriverEntryPoints
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

type GetMethodStubDelegate a = BStr -> BStr -> BStr -> FunPtr a
foreign import ccall "dynamic" makeGetMethodStubDelegate :: FunPtr (GetMethodStubDelegate a) -> GetMethodStubDelegate a


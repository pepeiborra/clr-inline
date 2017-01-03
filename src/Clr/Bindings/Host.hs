module Clr.Bindings.Host where

import Clr.Host
import Clr.Marshal
import Foreign.Ptr

-- | @'unsafeGetPointerToMethod' m@ returns a function pointer to the method @m@
--  as implemented in the Salsa .NET driver assembly (Salsa.dll).  It is safe only
--  if the type of the resulting function pointer matches that of the method given.
unsafeGetPointerToMethod :: String -> IO (FunPtr a)
unsafeGetPointerToMethod methodName = do
  result <- marshal methodName $ \methodName' -> getPointerToMethodRaw >>= \f-> f methodName'
  if result == nullFunPtr
    then error $ "Unable to execute Salsa.dll method '" ++ methodName ++ "'."
    else return result

getPointerToMethodRaw :: IO (GetPtrToMethod a)
getPointerToMethodRaw = getPtrToMethod_get >>= return . makeGetPtrToMethod

foreign import ccall "dynamic" makeGetPtrToMethod :: GetPtrToMethodFunPtr a -> GetPtrToMethod a


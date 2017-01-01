module Clr.Host.Box where

import Clr.Host
import Foreign.C.String
import Foreign.Ptr

-- | @'getBoxStub' t@ returns a function pointer to a function that, when
--   called, returns a boxed object reference to the given type.
getBoxStub :: String -> IO (FunPtr f)
getBoxStub typeName = withCString typeName $ \typeName'-> do
  stub <- getBoxStubRaw
  return $ stub typeName'

getBoxStubRaw :: IO (GetBoxStubDelegate a)
getBoxStubRaw = unsafeGetPointerToMethod "GetBoxStub" >>= return . makeGetBoxStubDelegate

type GetBoxStubDelegate a = CString -> FunPtr a
foreign import ccall "dynamic" makeGetBoxStubDelegate :: FunPtr (GetBoxStubDelegate a) -> GetBoxStubDelegate a








module Clr.Host.DotNet.IUnknown where

-- | 'queryInterface' calls the QueryInterface method of the given COM interface.
queryInterface this iid = do
  f <- getInterfaceFunction 0 {- QueryInterface -} makeQueryInterface this
  with (nullPtr :: InterfacePtr) $ \interfacePtr -> do
    with iid $ \refIID -> f this refIID interfacePtr >>= checkHR "IUnknown.QueryInterface"
    peek interfacePtr

type QueryInterface = InterfacePtr -> Ptr IID -> Ptr InterfacePtr -> IO HResult
foreign import stdcall "dynamic" makeQueryInterface :: FunPtr QueryInterface -> QueryInterface


-- | 'release' calls the Release method of the given COM interface.
release this = do
  f <- getInterfaceFunction 2 {- Release -} makeRelease this
  f this

type Release = InterfacePtr -> IO Word32
foreign import stdcall "dynamic" makeRelease :: FunPtr Release -> Release


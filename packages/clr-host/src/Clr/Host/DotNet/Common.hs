module Clr.Host.DotNet.Common where

import Clr.Host.DotNet.Guid
import Control.Exception (bracket)
import Data.Int
import Data.Word
import Foreign.Marshal
import Foreign.Ptr
import Foreign.Storable
import Text.Printf

-- | 'InterfacePtr' is a pointer to an arbitrary COM interface (which is a pointer to
--   a vtable of function pointers for the interface methods).
type InterfacePtr = Ptr (Ptr (FunPtr ()))

-- | 'withInterface i f' is like 'bracket' but for COM interface pointers, it calls
--   'release' once the computation is finished.
withInterface :: (IO InterfacePtr) -> (InterfacePtr -> IO a) -> IO a
withInterface i = bracket i (\x -> if x == nullPtr then return 0 else release x)


-- | 'getInterfaceFunction' @i makeFun obj@ is an action that returns the @i@th function
--   of the COM interface referred to by @obj@.  The function is returned as a Haskell
--   function by passing it through @makeFun@.
getInterfaceFunction :: Int -> (FunPtr a -> b) -> InterfacePtr -> IO b
getInterfaceFunction index makeFun this = do
  -- Obtain a pointer to the appropriate element in the vtable for this interface
  funPtr <- peek this >>= (flip peekElemOff) index
  -- Cast the function pointer to the expected type, and import it as a Haskell function
  return $ makeFun $ castFunPtr funPtr


foreign import stdcall "CoInitializeEx" coInitializeEx :: Ptr () -> Int32 -> IO HResult
foreign import stdcall "CoUninitialize" coUninitialize :: IO ()

coInit_MultiThreaded     = 0 :: Int32
coInit_ApartmentThreaded = 2 :: Int32


--
-- HResult Support
--

type HResult  = Word32

checkHR :: String -> HResult -> IO HResult
checkHR msg 0 = return 0
checkHR msg r = error $ printf "%s failed (0x%8x)" msg r

--
-- IUnknown
--

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


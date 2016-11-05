module Clr.Host.DotNet where

import Data.Word
import Data.Int
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal
import Foreign.C.String
import System.Win32
import System.IO
import Control.Exception (bracket)


-- | 'InterfacePtr' is a pointer to an arbitrary COM interface (which is a pointer to
--   a vtable of function pointers for the interface methods).
type InterfacePtr = Ptr (Ptr (FunPtr ()))

type ICorRuntimeHost = InterfacePtr
type ICLRMetaHost    = InterfacePtr
type ICLRRuntimeInfo = InterfacePtr
type IEnumUnknown    = InterfacePtr


startHostDotNet :: IO ()
startHostDotNet = return ()

stopHostDotNet :: IO ()
stopHostDotNet = return ()


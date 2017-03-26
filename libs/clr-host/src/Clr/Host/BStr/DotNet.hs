module Clr.Host.BStr.DotNet where

import Clr.Host.BStr.Type
import Control.Exception (bracket)
import Data.Word
import Foreign.C
import Foreign.Ptr

sysAllocString :: String -> IO BStr
sysAllocString s = withCWString s prim_SysAllocString
foreign import stdcall "oleauto.h SysAllocString" prim_SysAllocString :: CWString -> IO BStr

sysAllocStringLen :: Ptr Word16 -> CUInt -> IO BStr
sysAllocStringLen s l = prim_SysAllocStringLen (castPtr s) l
foreign import stdcall "oleauto.h SysAllocStringLen" prim_SysAllocStringLen :: CWString -> CUInt -> IO BStr

sysFreeString :: BStr -> IO ()
sysFreeString = prim_SysFreeString
foreign import stdcall "oleauto.h SysFreeString" prim_SysFreeString :: BStr -> IO ()

withBStr :: String -> (BStr -> IO a) -> IO a
withBStr s = bracket (sysAllocString s) (sysFreeString)


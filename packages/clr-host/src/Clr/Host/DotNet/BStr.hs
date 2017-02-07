module Clr.Host.DotNet.BStr where

import Control.Exception (bracket)
import Foreign.C.String
import Foreign.Ptr

newtype BStr = BStr (Ptr ()) deriving (Show, Eq)

sysAllocString :: String -> IO BStr
sysAllocString s = withCWString s prim_SysAllocString
foreign import stdcall "oleauto.h SysAllocString" prim_SysAllocString :: CWString -> IO BStr

sysFreeString :: BStr -> IO ()
sysFreeString = prim_SysFreeString
foreign import stdcall "oleauto.h SysFreeString" prim_SysFreeString :: BStr -> IO ()

withBStr :: String -> (BStr -> IO a) -> IO a
withBStr s = bracket (sysAllocString s) (sysFreeString)


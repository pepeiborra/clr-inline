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

type GetPtrToMethod a = FunPtr (CString -> IO (FunPtr a))

foreign import stdcall "clrHost.c getPointerToMethod_set" getPtrToMethod_set :: GetPtrToMethod a -> IO ()
foreign import stdcall "clrHost.c getPointerToMethod_get" getPtrToMethod_get :: IO (GetPtrToMethod a)

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


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
import Data.Word

type GetPtrToMethod a       = Ptr Word16 -> IO (FunPtr a)
type GetPtrToMethodFunPtr a = FunPtr (GetPtrToMethod a)

foreign import ccall "clrHost.c getPointerToMethod_set" getPtrToMethod_set :: GetPtrToMethodFunPtr a -> IO ()
foreign import ccall "clrHost.c getPointerToMethod_get" getPtrToMethod_get :: IO (GetPtrToMethodFunPtr a)

startClr :: IO ()
startClr = do
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


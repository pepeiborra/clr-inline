{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables #-}

module Clr.Host where

import Clr.Marshal

import Clr.Host.Config
#ifdef HAVE_MONO
import Clr.Host.Mono
#endif
#ifdef HAVE_DOTNET
import Clr.Host.DotNet
#endif

import Clr.Host.BStr
import Clr.Host.DriverEntryPoints

import Data.Coerce
import Data.Word
import Foreign.Ptr


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






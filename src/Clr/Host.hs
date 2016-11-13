{-# LANGUAGE CPP #-}

module Clr.Host where

import Clr.Host.Config
#ifdef HAVE_MONO
import Clr.Host.Mono
#endif
#ifdef HAVE_DOTNET
import Clr.Host.DotNet
#endif

startClr :: IO ()
startClr = do
  putStrLn "startClr"
  ClrHostConfig hostType <- getClrHostConfig
  case hostType of
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

stopClr :: IO ()
stopClr = putStrLn "stopClr"


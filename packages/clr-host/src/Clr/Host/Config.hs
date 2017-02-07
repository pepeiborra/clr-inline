{-# LANGUAGE CPP #-}

module Clr.Host.Config where

data ClrHostType = ClrHostMono | ClrHostDotNet

data ClrHostConfig = ClrHostConfig ClrHostType

#if defined (HAVE_MONO)
defaultHostConfig = ClrHostConfig ClrHostMono
#elif defined (HAVE_DOTNET)
defaultHostConfig = ClrHostConfig ClrHostDotNet
#endif

getClrHostConfig :: IO ClrHostConfig
getClrHostConfig = return defaultHostConfig


{-# LANGUAGE CPP #-}

module Clr.Host.BStr
  ( BStr(..)
  , allocBStr
  , freeBStr
  , withBStr
  ) where

import Control.Exception (bracket)

import Clr.Host.Config
import Clr.Host.BStr.Type

#ifdef HAVE_MONO
import Clr.Host.BStr.Mono(mono_ptr_to_bstr, mono_free_bstr)
#endif

#ifdef HAVE_DOTNET
import Clr.Host.BStr.DotNet(sysAllocStringLen, sysFreeString)
#endif

import Data.Word
import Foreign.Ptr

allocBStr :: (Integral len) => Ptr Word16 -> len -> IO BStr
allocBStr p l = do
  ClrHostConfig hostType <- getClrHostConfig
  case hostType of
#ifdef HAVE_MONO
    ClrHostMono   -> mono_ptr_to_bstr p (fromIntegral l)
#else
    ClrHostMono   -> error "not built with Mono support enabled"
#endif
#ifdef HAVE_DOTNET
    ClrHostDotNet -> sysAllocStringLen p (fromIntegral l)
#else
    ClrHostDotNet -> error "not built with .Net support enabled"
#endif


freeBStr :: BStr -> IO ()
freeBStr x = do
  ClrHostConfig hostType <- getClrHostConfig
  case hostType of
#ifdef HAVE_MONO
    ClrHostMono   -> mono_free_bstr x
#else
    ClrHostMono   -> error "not built with Mono support enabled"
#endif
#ifdef HAVE_DOTNET
    ClrHostDotNet -> sysFreeString x
#else
    ClrHostDotNet -> error "not built with .Net support enabled"
#endif

withBStr :: (Integral len) => Ptr Word16 -> len -> (BStr-> IO a) -> IO a
withBStr p l = bracket (allocBStr p l) freeBStr



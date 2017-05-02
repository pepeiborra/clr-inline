{-# LANGUAGE CPP, BangPatterns, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses #-}

module Clr.Host.BStr
  ( BStr(..)
  , allocBStr
  , freeBStr
  , withBStr
  ) where

import Control.Exception (bracket)
import Data.Coerce
import Data.Word
import Foreign.Ptr
import Foreign.Storable

import Clr.Host.Config
import Clr.Host.BStr.Type

#ifdef HAVE_MONO
import Clr.Host.BStr.Mono(mono_ptr_to_bstr_hask, mono_free_bstr)
#endif

#ifdef HAVE_DOTNET
import Clr.Host.BStr.DotNet(sysAllocStringLen, sysFreeString)
#endif

import Clr.Marshal

import Data.Text
import Data.Text.Foreign

allocBStr :: (Integral len) => Ptr Word16 -> len -> IO BStr
allocBStr p l = do
  ClrHostConfig hostType <- getClrHostConfig
  case hostType of
#ifdef HAVE_MONO
    ClrHostMono   -> mono_ptr_to_bstr_hask p (fromIntegral l)
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

instance Marshal Text BStr where
  marshal x f = do
    bstr <- useAsPtr x (\p-> \l-> allocBStr p l)
    !res <- f bstr
    freeBStr bstr
    return res

instance Marshal String BStr where
  marshal x f = marshal (pack x) f

instance Unmarshal BStr Text where
  unmarshal x = do
    let charSize = 2
    let ptrData   = coerce x              :: Ptr Word16
    let ptrLen    = plusPtr ptrData (-4)  :: Ptr Word16
    lenBytes     <- peek ptrLen
    !t <- fromPtr ptrData $ fromIntegral $ lenBytes `div` charSize
    freeBStr x
    return t

instance Unmarshal BStr String where
  unmarshal x = unmarshal x >>= return . unpack



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

bstrToPtrData :: BStr -> Ptr Word16
bstrToPtrData = coerce

bstrToPtrLen :: BStr -> Ptr Word16
bstrToPtrLen bstr = plusPtr (bstrToPtrData bstr) (-4)

bstrLenBytes :: BStr -> IO Word16
bstrLenBytes = peek . bstrToPtrLen

bstrLenChars :: BStr -> IO Word16
bstrLenChars bstr = do
  let charSize = 2
  lenBytes <- bstrLenBytes bstr
  return $ lenBytes `div` charSize

bstrToText :: BStr -> IO Text
bstrToText bstr = do
  let ptrData = bstrToPtrData bstr
  lenChars   <- bstrLenChars  bstr
  fromPtr ptrData $ fromIntegral lenChars

instance {-# OVERLAPPING #-} Marshal Text BStr where
  marshal x f = do
    bstr <- useAsPtr x (\p-> \l-> allocBStr p l)
    !res <- f bstr
    freeBStr bstr
    return res

instance {-# OVERLAPPING #-} Marshal String BStr where
  marshal x f = marshal (pack x) f

instance {-# OVERLAPPING #-} Marshal BStr Text where
  marshal x f = bstrToText x >>= f

instance {-# OVERLAPPING #-} Marshal BStr String where
  marshal x f = marshal x $ \t-> f (unpack t)

instance {-# OVERLAPPING #-} Unmarshal BStr Text where
  unmarshal x = do
    !t <- bstrToText x
    freeBStr x
    return t

instance {-# OVERLAPPING #-} Unmarshal BStr String where
  unmarshal x = unpack <$> unmarshal x

instance {-# OVERLAPPING #-} Unmarshal Text BStr where
  unmarshal x = useAsPtr x (\p-> \l-> allocBStr p l)

instance {-# OVERLAPPING #-} Unmarshal String BStr where
  unmarshal x = unmarshal $ pack x


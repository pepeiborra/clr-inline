{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE KindSignatures, GADTs, TypeInType #-}



module Clr.Host.GCHandle where

import Clr.Host.DriverEntryPoints
import Foreign.Ptr

newtype GCHandle typ = GCHandle (Ptr Int)

-- | Releases the .NET object indicated by the given object id.
releaseObject :: GCHandle t -> IO ()
releaseObject handle = unsafeGetPointerToMethod "ReleaseObject" >>= return . makeReleaseObjectDelegate >>= \f-> f handle

type ReleaseObjectDelegate t = GCHandle t -> IO ()
foreign import ccall "dynamic" makeReleaseObjectDelegate :: FunPtr (ReleaseObjectDelegate t) -> ReleaseObjectDelegate t


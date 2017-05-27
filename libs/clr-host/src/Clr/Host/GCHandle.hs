{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE GADTs, TypeInType #-}



module Clr.Host.GCHandle where

import Clr.Host.DriverEntryPoints

import Data.Coerce
import Foreign.Ptr

newtype GCHandle typ = GCHandle (Ptr Int)

-- | Releases the .NET object indicated by the given object id.
releaseObject :: GCHandle t -> IO ()
releaseObject handle = fmap makeReleaseObjectDelegate gcHandleFinalizer >>= \f-> f $ coerce handle

type ReleaseObjectDelegate = Ptr Int -> IO ()
foreign import ccall "dynamic" makeReleaseObjectDelegate :: FunPtr ReleaseObjectDelegate -> ReleaseObjectDelegate

gcHandleFinalizer :: IO (FunPtr (Ptr Int -> IO ()))
gcHandleFinalizer = unsafeGetPointerToMethod "ReleaseObject"

newHandle :: GCHandle t -> IO (GCHandle t)
newHandle x = do
  f <- fmap makeNewHandleDelegate (unsafeGetPointerToMethod "NewHandle")
  y <- f $ coerce x
  return $ coerce y

type NewHandleDelegate = Ptr Int -> IO (Ptr Int)
foreign import ccall "dynamic" makeNewHandleDelegate :: FunPtr NewHandleDelegate -> NewHandleDelegate


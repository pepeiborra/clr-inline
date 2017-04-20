module Clr.Bindings
  ( module Clr.Bindings.Box
  , module Clr.Bindings.DynImports
  , module Clr.Bindings.BStr
  , BStr(..)
  , saveDynamicAssembly
  ) where


import Clr.Marshal

import Clr.Host
import Clr.Host.BStr

import Clr.Bindings.Box
import Clr.Bindings.DynImports
import Clr.Marshal.Host
import Clr.Bindings.BStr
import Clr.Bindings.Object

import Data.Word
import Foreign.Ptr


-- | 'saveDynamicAssembly' saves the assembly containing the dynamically-generated
--   wrapper stubs to disk (for debugging purposes).
saveDynamicAssembly :: IO ()
saveDynamicAssembly = unsafeGetPointerToMethod "SaveDynamicAssembly" >>=  makeSaveDynamicAssemblyDelegate

type SaveDynamicAssemblyDelegate = IO ()
foreign import ccall "dynamic" makeSaveDynamicAssemblyDelegate :: FunPtr SaveDynamicAssemblyDelegate -> SaveDynamicAssemblyDelegate





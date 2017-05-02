module Clr.Bindings
  ( module Clr.Bindings.Box
  , module Clr.Bindings.BStr
  , module Clr.Bindings.DynImports
  , module Clr.Bindings.Object
  , BStr(..)
  , saveDynamicAssembly
  ) where


import Clr.Marshal

import Clr.Host
import Clr.Host.BStr

import Clr.Bindings.Box
import Clr.Bindings.BStr
import Clr.Bindings.DynImports
import Clr.Bindings.Object

import Data.Word
import Foreign.Ptr


-- | 'saveDynamicAssembly' saves the assembly containing the dynamically-generated
--   wrapper stubs to disk (for debugging purposes).
saveDynamicAssembly :: IO ()
saveDynamicAssembly = unsafeGetPointerToMethod "SaveDynamicAssembly" >>=  makeSaveDynamicAssemblyDelegate

type SaveDynamicAssemblyDelegate = IO ()
foreign import ccall "dynamic" makeSaveDynamicAssemblyDelegate :: FunPtr SaveDynamicAssemblyDelegate -> SaveDynamicAssemblyDelegate





{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, UndecidableInstances #-}

module Clr.Bindings.WrapImports where

import Clr.Object
import Clr.Bridge
import Clr.Delegate
import Clr.TypeString

import Clr.Host
import Clr.Host.BStr
import Clr.Host.GCHandle
import Clr.Host.Delegate

import Clr.Marshal

import Data.Kind
import Foreign.Ptr

--
-- WrapperImportType t is the type of a function from a `t` to a `FunPtr t`
--
type family WrapperImportType t where
  WrapperImportType t = t -> IO (FunPtr t)

class WrapperImport t where
  wrapperImport :: WrapperImportType (DelegateBridgeType t)

instance ( TString t
         , Delegate t
         , BridgeType t ~ GCHandle t
         , DelegateArgTypes t ~ '[]
         , WrapperImport t
         ) => DelegateConstructor0 t where
  rawConstructDelegate0 f = do
    delCtor <- getDelegateConstructorStub (tString @t) (wrapperImport @t)
    delCtor f

instance ( TString t
         , Delegate t
         , BridgeType t ~ GCHandle t
         , DelegateArgTypes t ~ '[ a0 ]
         , WrapperImport t
         ) => DelegateConstructor1 t where
  rawConstructDelegate1 f = do
    delCtor <- getDelegateConstructorStub (tString @t) (wrapperImport @t)
    delCtor f

instance ( TString t
         , Delegate t
         , BridgeType t ~ GCHandle t
         , DelegateArgTypes t ~ '[ a0, a1 ]
         , WrapperImport t
         ) => DelegateConstructor2 t where
  rawConstructDelegate2 f = do
    delCtor <- getDelegateConstructorStub (tString @t) (wrapperImport @t)
    delCtor f



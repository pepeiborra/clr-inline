{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, UndecidableInstances #-}

module Clr.Bindings.DynImports where

import Clr.Bridge
import Clr.Method.Instance
import Clr.Method.Static
import Clr.TypeString

import Clr.Bindings.Host

import Data.Kind
import Foreign.Ptr

--
-- DynamicImportType t is the type of a function from a `FunPtr t` to a `t`
--
type family DynamicImportType t where
  DynamicImportType t = FunPtr t -> t

--
-- IgnoreUnitType a r is the type of a function from `a` to `r`,
-- unless `a` is `()`, in which case it evaluates to just `r`
--
type family IngoreUnitType a r where
  IngoreUnitType () r = r
  IngoreUnitType x  r = x -> r

--
-- HandleUnit does the term level application of an argument,
-- providing that argument is not unit
--
class HandleUnit f x r where
  handleUnit :: f -> x -> r

instance HandleUnit a () a where
  handleUnit a () = a

instance HandleUnit (x -> r) x r where
  handleUnit f x = f x

--
-- Each method signature needs its own seperate `foreign import "dynamic"`,
-- which is specified by implementing a MethodDynImport class
--
class MethodDynImportI1 t m arg0 where
  methodDynImportI1 :: DynamicImportType (BridgeType t -> IngoreUnitType (BridgeType arg0) (IO (BridgeTypeM (ResultTypeI1 t m arg0))))

class MethodDynImportI2 t m arg0 arg1 where
  methodDynImportI2 :: DynamicImportType (BridgeType t -> BridgeType arg0 -> BridgeType arg1 -> IO (BridgeTypeM (ResultTypeI2 t m arg0 arg1)))

class MethodDynImportI3 t m arg0 arg1 arg2 where
  methodDynImportI3 :: DynamicImportType (BridgeType t -> BridgeType arg0 -> BridgeType arg1 -> BridgeType arg2 -> IO (BridgeTypeM (ResultTypeI3 t m arg0 arg1 arg2)))

class MethodDynImportS1 t m arg0 where
  methodDynImportS1 :: DynamicImportType (IngoreUnitType (BridgeType arg0) (IO (BridgeTypeM (ResultTypeS1 t m arg0))))

class MethodDynImportS2 t m arg0 arg1 where
  methodDynImportS2 :: DynamicImportType (BridgeType arg0 -> BridgeType arg1 -> IO (BridgeTypeM (ResultTypeS2 t m arg0 arg1)))

class MethodDynImportS3 t m arg0 arg1 arg2 where
  methodDynImportS3 :: DynamicImportType (BridgeType arg0 -> BridgeType arg1 -> BridgeType arg2 -> IO (BridgeTypeM (ResultTypeS3 t m arg0 arg1 arg2)))

--
--
--
instance ( TString t
         , TString m
         , TString arg0
         , MethodResultI1 t m arg0
         , MethodDynImportI1 t m arg0
         , HandleUnit (IngoreUnitType (BridgeType arg0) (IO (BridgeTypeM (ResultTypeI1 t m arg0)))) (BridgeType arg0) (IO (BridgeTypeM (ResultTypeI1 t m arg0)))
         ) => MethodInvokeI1 t m arg0 where
  rawInvokeI1 obj x = do
    fp   <- getMethodStub (tString @t) (tString @m) (tString @arg0)
    let f = methodDynImportI1 @t @m @arg0 fp
    handleUnit @(IngoreUnitType (BridgeType arg0) (IO (BridgeTypeM (ResultTypeI1 t m arg0)))) @(BridgeType arg0) @(IO (BridgeTypeM (ResultTypeI1 t m arg0))) (f obj) x

instance ( TString t
         , TString m
         , TString arg0
         , TString arg1
         , MethodResultI2 t m arg0 arg1
         , MethodDynImportI2 t m arg0 arg1
         ) => MethodInvokeI2 t m arg0 arg1 where
  rawInvokeI2 obj x y = getMethodStub (tString @t) (tString @m) ((tString @arg0) ++ ";" ++ (tString @arg1)) >>= return . (methodDynImportI2 @t @m @arg0 @arg1) >>= \f-> f obj x y

instance ( TString t
         , TString m
         , TString arg0
         , TString arg1
         , TString arg2
         , MethodResultI3 t m arg0 arg1 arg2
         , MethodDynImportI3 t m arg0 arg1 arg2
         ) => MethodInvokeI3 t m arg0 arg1 arg2 where
  rawInvokeI3 obj x y z = getMethodStub (tString @t) (tString @m) ((tString @arg0) ++ ";" ++ (tString @arg1) ++ ";" ++ (tString @arg2)) >>= return . (methodDynImportI3 @t @m @arg0 @arg1 @arg2) >>= \f-> f obj x y z

instance ( TString t
         , TString m
         , TString arg0
         , MethodResultS1 t m arg0
         , MethodDynImportS1 t m arg0
         , HandleUnit (IngoreUnitType (BridgeType arg0) (IO (BridgeTypeM (ResultTypeS1 t m arg0)))) (BridgeType arg0) (IO (BridgeTypeM (ResultTypeS1 t m arg0)))
         ) => MethodInvokeS1 t m arg0 where
  rawInvokeS1 x = do
    fp   <- getMethodStub (tString @t) (tString @m) (tString @arg0)
    let f = methodDynImportS1 @t @m @arg0 fp
    handleUnit @(IngoreUnitType (BridgeType arg0) (IO (BridgeTypeM (ResultTypeS1 t m arg0)))) @(BridgeType arg0) @(IO (BridgeTypeM (ResultTypeS1 t m arg0))) f x

instance ( TString t
         , TString m
         , TString arg0
         , TString arg1
         , MethodResultS2 t m arg0 arg1
         , MethodDynImportS2 t m arg0 arg1
         ) => MethodInvokeS2 t m arg0 arg1 where
  rawInvokeS2 x y = getMethodStub (tString @t) (tString @m) ((tString @arg0) ++ ";" ++ (tString @arg1)) >>= return . (methodDynImportS2 @t @m @arg0 @arg1) >>= \f-> f x y

instance ( TString t
         , TString m
         , TString arg0
         , TString arg1
         , TString arg2
         , MethodResultS3 t m arg0 arg1 arg2
         , MethodDynImportS3 t m arg0 arg1 arg2
         ) => MethodInvokeS3 t m arg0 arg1 arg2 where
  rawInvokeS3 x y z = getMethodStub (tString @t) (tString @m) ((tString @arg0) ++ ";" ++ (tString @arg1) ++ ";" ++ (tString @arg2)) >>= return . (methodDynImportS3 @t @m @arg0 @arg1 @arg2) >>= \f-> f x y z



{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, UndecidableInstances #-}

module Clr.Bindings.Stubs where

import Clr.Bridge
import Clr.Method.Instance
import Clr.Method.Static
import Clr.TypeString

import Clr.Bindings.Host

import Data.Kind
import Foreign.Ptr


type family DynamicStubType t where
  DynamicStubType t = FunPtr t -> t

type family IngoreUnitType a r where
  IngoreUnitType () r = r
  IngoreUnitType x  r = x -> r

class HandleUnit x f r where
  handleUnit :: f -> x -> r

instance HandleUnit () a a where
  handleUnit a () = a

instance HandleUnit x (x -> r) r where
  handleUnit f x = f x

class MethodI1' t m arg0 where
  type ResultTypeI1' t m arg0 :: Maybe Type
  makerFuncI1 :: DynamicStubType (BridgeType t -> IngoreUnitType (BridgeType arg0) (IO (BridgeTypeM (ResultTypeI1' t m arg0))))

class MethodI2' t m arg0 arg1 where
  type ResultTypeI2' t m arg0 arg1 :: Maybe Type
  makerFuncI2 :: DynamicStubType (BridgeType t -> BridgeType arg0 -> BridgeType arg1 -> IO (BridgeTypeM (ResultTypeI2' t m arg0 arg1)))

instance ( TString t
         , TString m
         , TString arg0
         , MethodI1' t m arg0
         , HandleUnit (BridgeType arg0) (IngoreUnitType (BridgeType arg0) (IO (BridgeTypeM (ResultTypeI1' t m arg0)))) (IO (BridgeTypeM (ResultTypeI1' t m arg0)))
         ) => MethodI1 t m arg0 where
  type ResultTypeI1 t m arg0 = ResultTypeI1' t m arg0
  rawInvokeI1 obj x = do
    fp <- getMethodStub (tString @t) (tString @m) (tString @arg0)
    let f = makerFuncI1 @t @m @arg0 fp
    handleUnit @(BridgeType arg0) @(IngoreUnitType (BridgeType arg0) (IO (BridgeTypeM (ResultTypeI1' t m arg0)))) @(IO (BridgeTypeM (ResultTypeI1' t m arg0))) (f obj) x

instance (TString t, TString m, TString arg0, TString arg1, MethodI2' t m arg0 arg1) => MethodI2 t m arg0 arg1 where
  type ResultTypeI2 t m arg0 arg1 = ResultTypeI2' t m arg0 arg1
  rawInvokeI2 obj x y = getMethodStub (tString @t) (tString @m) ((tString @arg0) ++ ";" ++ (tString @arg1)) >>= return . (makerFuncI2 @t @m @arg0 @arg1) >>= \f-> f obj x y

class MethodS1' t m arg0 where
  type ResultTypeS1' t m arg0 :: Maybe Type
  makerFuncS1 :: DynamicStubType (IngoreUnitType (BridgeType arg0) (IO (BridgeTypeM (ResultTypeS1' t m arg0))))

class MethodS2' t m arg0 arg1 where
  type ResultTypeS2' t m arg0 arg1 :: Maybe Type
  makerFuncS2 :: DynamicStubType (BridgeType arg0 -> BridgeType arg1 -> IO (BridgeTypeM (ResultTypeS2' t m arg0 arg1)))

class MethodS3' t m arg0 arg1 arg2 where
  type ResultTypeS3' t m arg0 arg1 arg2 :: Maybe Type
  makerFuncS3 :: DynamicStubType (BridgeType arg0 -> BridgeType arg1 -> BridgeType arg2 -> IO (BridgeTypeM (ResultTypeS3' t m arg0 arg1 arg2)))

instance ( TString t
         , TString m
         , TString arg0
         , MethodS1' t m arg0
         , HandleUnit (BridgeType arg0) (IngoreUnitType (BridgeType arg0) (IO (BridgeTypeM (ResultTypeS1' t m arg0)))) (IO (BridgeTypeM (ResultTypeS1' t m arg0)))
         ) => MethodS1 t m arg0 where
  type ResultTypeS1 t m arg0 = ResultTypeS1' t m arg0
  rawInvokeS1 x = do
    fp <- getMethodStub (tString @t) (tString @m) (tString @arg0)
    let f = makerFuncS1 @t @m @arg0 fp
    handleUnit @(BridgeType arg0) @(IngoreUnitType (BridgeType arg0) (IO (BridgeTypeM (ResultTypeS1' t m arg0)))) @(IO (BridgeTypeM (ResultTypeS1' t m arg0))) f x

instance (TString t, TString m, TString arg0, TString arg1, MethodS2' t m arg0 arg1) => MethodS2 t m arg0 arg1 where
  type ResultTypeS2 t m arg0 arg1 = ResultTypeS2' t m arg0 arg1
  rawInvokeS2 x y = getMethodStub (tString @t) (tString @m) ((tString @arg0) ++ ";" ++ (tString @arg1)) >>= return . (makerFuncS2 @t @m @arg0 @arg1) >>= \f-> f x y

instance (TString t, TString m, TString arg0, TString arg1, TString arg2, MethodS3' t m arg0 arg1 arg2) => MethodS3 t m arg0 arg1 arg2 where
  type ResultTypeS3 t m arg0 arg1 arg2 = ResultTypeS3' t m arg0 arg1 arg2
  rawInvokeS3 x y z = getMethodStub (tString @t) (tString @m) ((tString @arg0) ++ ";" ++ (tString @arg1) ++ ";" ++ (tString @arg2)) >>= return . (makerFuncS3 @t @m @arg0 @arg1 @arg2) >>= \f-> f x y z



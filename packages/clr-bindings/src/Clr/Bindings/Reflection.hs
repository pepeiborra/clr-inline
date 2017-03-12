{-# LANGUAGE TypeApplications, TypeInType, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Clr.Bindings.Reflection where

import Clr

import Clr.Host
import Clr.Host.BStr

import Clr.Bindings.Box
import Clr.Bindings.Host

import Foreign.Ptr
import Data.Word
import qualified Data.Text as T

type T_Assembly = T "System.Reflection.Assembly" '[]
type Assembly   = Object T_Assembly

type T_ClrType  = T "System.Type" '[]
type T_ClrTypeArray = T "System.Type[]" '[]

--
-- System.Reflection.Assembly.Load
--
instance MethodS1 (T_Assembly) (T "Load" '[]) (T_string) where
  type ResultTypeS1 (T_Assembly) (T "Load" '[]) (T_string) = 'Just (T_Assembly)
  rawInvokeS1 x = getMethodStub "System.Reflection.Assembly, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "Load" "System.String" >>= return . makeLoad >>= \f-> f x

foreign import ccall "dynamic" makeLoad :: FunPtr (BStr -> IO (ObjectID T_Assembly)) -> (BStr -> IO (ObjectID T_Assembly))

type instance Candidates (T_Assembly) (T "Load" '[]) = '[ '[ T_string ] ]

assemblyLoad :: T.Text -> IO Assembly
assemblyLoad assemName = invokeS @"Load" @T_Assembly assemName

--
-- System.Reflection.Assembly.GetTypes
--



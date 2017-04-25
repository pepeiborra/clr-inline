{-# LANGUAGE TypeInType, TypeApplications, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Clr.Bindings.Object where

import Clr
import Clr.TypeString

import Clr.Host
import Clr.Host.BStr

import Clr.Bindings.DynImports
import Clr.Marshal.Host
import Clr.Bindings.BStr

import Foreign.Ptr


type T_ToString = T "ToString" '[]

type instance SuperTypes T_object = '[ ]

type instance Members T_object = '[ T_ToString, T "GetType" '[] ]

type instance Candidates T_object  T_ToString       = '[ '[ ] ]
type instance Candidates T_object (T "GetType" '[]) = '[ '[ ] ]

foreign import ccall "dynamic" makeToString :: FunPtr (ObjectID t -> IO BStr) -> (ObjectID t -> IO BStr)
foreign import ccall "dynamic" makeGetType  :: FunPtr (ObjectID t -> IO (ObjectID (T "System.Type" '[]))) -> (ObjectID t -> IO (ObjectID (T "System.Type" '[])))

instance MethodResultI1 T_object T_ToString () where
  type ResultTypeI1 T_object T_ToString () = 'Just T_string

instance MethodResultI1 T_object (T "GetType" '[]) () where
  type ResultTypeI1 T_object (T "GetType" '[]) () = 'Just (T "System.Type" '[])

instance MethodDynImportI1 T_object T_ToString () where
  methodDynImportI1 = makeToString

instance MethodDynImportI1 T_object (T "GetType" '[]) () where
  methodDynImportI1 = makeGetType


{-# LANGUAGE TypeInType, TypeApplications, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Clr.Bindings.Object where

import Clr
import Clr.TypeString

import Clr.Host
import Clr.Host.BStr

import Clr.Bindings.Host
import Clr.Bindings.Marshal

import Foreign.Ptr


type T_ToString = T "ToString" '[]

type instance SuperTypes T_object = '[ ]

type instance Members T_object = '[ T_ToString ]

type instance Candidates T_object  T_ToString = '[ '[ ] ]

foreign import ccall "dynamic" makeToString :: FunPtr (ObjectID t -> IO BStr) -> (ObjectID t -> IO BStr)

instance MethodI1 T_object T_ToString () where
  type ResultTypeI1 T_object T_ToString () = 'Just T_string
  rawInvokeI1 obj () = getMethodStub (tString @T_object) (tString @T_ToString) (tString @()) >>= return . makeToString >>= \f-> f obj


{-# LANGUAGE TypeInType, TypeApplications, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Clr.Bindings.IEnumerable where

import Clr
import Clr.Host
import Clr.Bindings.Host
import Foreign.Ptr

type T_IEnumerable t = T "System.Collectios.Generic.IEnumerable" '[t]
type T_IEnumerator t = T "System.Collectios.Generic.IEnumerator" '[t]

instance MethodI1 (T_IEnumerable t) (T "GetEnumerator" '[]) () where
  type ResultTypeI1 (T_IEnumerable t) (T "GetEnumerator" '[]) () = 'Just (T_IEnumerator t)
  rawInvokeI1 ienumerable _ = getMethodStub "System.Collections.Generic.IEnumerable`1, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "GetEnumerator" "" >>= return . makeGetEnumerator >>= \f-> f ienumerable

foreign import ccall "dynamic" makeGetEnumerator :: FunPtr (ObjectID a -> IO (ObjectID b)) -> (ObjectID a -> IO (ObjectID b))

type instance Members (T_IEnumerable t) = '[ T "GetEnumerator" '[] ]

type instance Candidates (T_IEnumerable t) (T "GetEnumerator" '[]) = '[ '[] ]

getEnumerator :: Object (T_IEnumerable t) -> IO (Object (T_IEnumerator t))
getEnumerator x = invokeI @"GetEnumerator" x ()

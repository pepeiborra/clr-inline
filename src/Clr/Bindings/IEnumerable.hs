{-# LANGUAGE TypeInType, TypeApplications, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Clr.Bindings.IEnumerable where

import Clr
import Clr.Host
import Clr.Bindings.Host
import Foreign.Ptr

instance MethodI1 (T "IEnumerable" '[t]) (T "GetEnumerator" '[]) () where
  type ResultTypeI1 (T "IEnumerable" '[t]) (T "GetEnumerator" '[]) () = 'Just (T "IEnumerator" '[t])
  rawInvokeI1 ienumerable _ = getMethodStub "System.Collections.Generic.IEnumerable`1, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "GetEnumerator" "" >>= return . makeGetEnumerator >>= \f-> f ienumerable

foreign import ccall "dynamic" makeGetEnumerator :: FunPtr (ObjectID a -> IO (ObjectID b)) -> (ObjectID a -> IO (ObjectID b))

type instance Members (T "IEnumerable" '[t]) = '[ T "GetEnumerator" '[] ]

type instance Candidates (T "IEnumerable" '[t]) (T "GetEnumerator" '[]) = '[ '[] ]

getEnumerator :: Object (T "IEnumerable" '[t]) -> IO (Object (T "IEnumerator" '[t]))
getEnumerator x = invokeI @"GetEnumerator" x ()

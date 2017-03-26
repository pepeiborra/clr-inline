{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeApplications #-}

module Main where

import Clr
import Clr.Host
import Clr.Bindings
import Clr.Bindings.IEnumerable

import Data.Int(Int32, Int64)
import Foreign.Ptr(Ptr, FunPtr)

import Pipes
import Pipes.Prelude

instance MethodS1 (T "System.Console" '[]) (T "WriteLine" '[]) () where
  type ResultTypeS1 (T "System.Console" '[]) (T "WriteLine" '[]) () = 'Nothing
  rawInvokeS1 x = getMethodStub "System.Console" "WriteLine" "" >>= return . makeWriteLineType0 >>= \f-> f

foreign import ccall "dynamic" makeWriteLineType0 :: FunPtr (IO ()) -> IO ()

instance MethodS1 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) where
  type ResultTypeS1 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) = 'Nothing
  rawInvokeS1 x = getMethodStub "System.Console" "WriteLine" "System.String" >>= return . makeWriteLineType1 >>= \f-> f x

foreign import ccall "dynamic" makeWriteLineType1 :: FunPtr (BStr -> IO ()) -> (BStr -> IO ())

instance MethodS1 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.Int32" '[]) where
  type ResultTypeS1 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.Int32" '[]) = 'Nothing
  rawInvokeS1 x = getMethodStub "System.Console" "WriteLine" "System.Int32" >>= return . makeWriteLineType2 >>= \f-> f x

foreign import ccall "dynamic" makeWriteLineType2 :: FunPtr (Int32 -> IO ()) -> (Int32 -> IO ())

instance MethodS2 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) (T "System.Object" '[]) where
  type ResultTypeS2 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) (T "System.Object" '[]) = 'Nothing
  rawInvokeS2 x y = getMethodStub "System.Console" "WriteLine" "System.String;System.Object" >>= return . makeWriteLineType3 >>= \f-> f x y

foreign import ccall "dynamic" makeWriteLineType3 :: FunPtr (BStr -> (ObjectID a) -> IO ()) -> (BStr -> (ObjectID a) -> IO ())

instance MethodS3 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) (T "System.Object" '[]) (T "System.Object" '[]) where
  type ResultTypeS3 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) (T "System.Object" '[]) (T "System.Object" '[]) = 'Nothing
  rawInvokeS3 x y z = getMethodStub "System.Console" "WriteLine" "System.String;System.Object;System.Object" >>= return . makeWriteLineType4 >>= \f-> f x y z

foreign import ccall "dynamic" makeWriteLineType4 :: FunPtr (BStr -> (ObjectID a) -> (ObjectID b) -> IO ()) -> (BStr -> (ObjectID a) -> (ObjectID b) -> IO ())

type instance Candidates (T "System.Console" '[]) (T "WriteLine" '[]) = '[ '[                                                                     ]
                                                                         , '[ T "System.String" '[]                                               ]
                                                                         , '[ T "System.Int32"  '[]                                               ]
                                                                         , '[ T "System.String" '[], T "System.Object" '[]                        ]
                                                                         , '[ T "System.String" '[], T "System.Object" '[], T "System.Object" '[] ] ]

type instance Candidates (T "System.Collections.Generic.List" '[t]) (T "System.Collections.Generic.List" '[t]) = '[ '[] ]

instance Constructor1 (T "System.Collections.Generic.List" '[t]) () where
  rawNew1 () = getMethodStub "System.Collections.Generic.List`1[System.String]" ".ctor" "" >>= return . makeListCTor >>= \f-> f

foreign import ccall "dynamic" makeListCTor :: FunPtr (IO (ObjectID a)) -> IO (ObjectID a)

type instance SuperTypes (T "System.Collections.Generic.List" '[t]) = '[ T "System.Collections.Generic.IEnumerable" '[t] ]

type instance Members (T "System.Collections.Generic.List" '[t]) = '[ T "Add" '[] ]
type instance Candidates (T "System.Collections.Generic.List" '[t]) (T "Add" '[]) = '[ '[t] ]

instance MethodI1 (T "System.Collections.Generic.List" '[T "System.String" '[]]) (T "Add" '[]) (T "System.String" '[]) where
  type ResultTypeI1 (T "System.Collections.Generic.List" '[T "System.String" '[]]) (T "Add" '[]) (T "System.String" '[]) = 'Nothing
  rawInvokeI1 l s = getMethodStub "System.Collections.Generic.List`1[System.String]" "Add" "System.String" >>= return . makeListAdd >>= \f-> f l s

foreign import ccall "dynamic" makeListAdd :: FunPtr (ObjectID a -> BStr -> IO ()) -> (ObjectID a -> BStr -> IO ())

main :: IO ()
main = do
  startClr
  invokeS @"WriteLine" @"System.Console" ()                                     -- Console.WriteLine()
  invokeS @"WriteLine" @"System.Console" "Hello CLR!!!"                         -- Console.WriteLine(String)
  invokeS @"WriteLine" @"System.Console" (2 :: Int32)                           -- Console.WriteLine(Int32)
  invokeS @"WriteLine" @"System.Console" ("The year is {0}", 2017::Int64)       -- Console.WriteLine(String, Object)
  invokeS @"WriteLine" @"System.Console" ("Well {0} {1}", "This", "Is Cool")    -- Console.WriteLine(String, Object, Object)
  list <- new @'("System.Collections.Generic.List", "System.String") ()         -- generics
  invokeI @"Add" list "foo"
  invokeI @"Add" list "bar"
  runEffect $ toProducer list >-> stdoutLn                                      -- IEnumerable implementors can be converted to Producers (pipes package)
  return ()


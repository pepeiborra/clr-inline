{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeApplications, ScopedTypeVariables #-}

module Main where

import Clr
import Clr.Host
import Clr.TypeString

import Clr.Bindings
import Clr.Bindings.IEnumerable
import Clr.Bindings.Reflection

import Data.Int(Int32, Int64)
import Foreign.Ptr(Ptr, FunPtr)

import Pipes
import Pipes.Prelude(stdoutLn)

type T_Console   = T "System.Console" '[]
type T_List t    = T "System.Collections.Generic.List" '[t]

type T_Add       = T "Add" '[]
type T_WriteLine = T "WriteLine" '[]

type instance SuperTypes (T_List t) = '[ T_IEnumerable t, T_IEnumerable', T_object ]

type instance Members (T_List t) = '[ T_Add ]

type instance Candidates (T_List t) T_Add      = '[ '[t] ]
type instance Candidates T_Console T_WriteLine = '[ '[                              ]
                                                  , '[ T_string                     ]
                                                  , '[ T_int                        ]
                                                  , '[ T_string, T_object           ]
                                                  , '[ T_string, T_object, T_object ] ]
type instance Candidates (T_List t) (T_List t) = '[ '[] ]


instance MethodS1 T_Console T_WriteLine () where
  type ResultTypeS1 T_Console T_WriteLine () = 'Nothing
  rawInvokeS1 x = getMethodStub (tString @T_Console) (tString @T_WriteLine) "" >>= return . makeWriteLineType0 >>= \f-> f

foreign import ccall "dynamic" makeWriteLineType0 :: FunPtr (IO ()) -> IO ()

instance MethodS1 T_Console T_WriteLine T_string where
  type ResultTypeS1 T_Console T_WriteLine T_string = 'Nothing
  rawInvokeS1 x = getMethodStub (tString @T_Console) (tString @T_WriteLine) (tString @T_string) >>= return . makeWriteLineType1 >>= \f-> f x

foreign import ccall "dynamic" makeWriteLineType1 :: FunPtr (BStr -> IO ()) -> (BStr -> IO ())

instance MethodS1 T_Console T_WriteLine T_int where
  type ResultTypeS1 T_Console T_WriteLine T_int = 'Nothing
  rawInvokeS1 x = getMethodStub (tString @T_Console) (tString @T_WriteLine) (tString @T_int) >>= return . makeWriteLineType2 >>= \f-> f x

foreign import ccall "dynamic" makeWriteLineType2 :: FunPtr (Int32 -> IO ()) -> (Int32 -> IO ())

instance MethodS2 T_Console T_WriteLine T_string T_object where
  type ResultTypeS2 T_Console T_WriteLine T_string T_object = 'Nothing
  rawInvokeS2 x y = getMethodStub (tString @T_Console) (tString @T_WriteLine) (tString @T_string ++ ";" ++ tString @T_object) >>= return . makeWriteLineType3 >>= \f-> f x y

foreign import ccall "dynamic" makeWriteLineType3 :: FunPtr (BStr -> (ObjectID a) -> IO ()) -> (BStr -> (ObjectID a) -> IO ())

instance MethodS3 T_Console T_WriteLine T_string (T_object) (T_object) where
  type ResultTypeS3 T_Console T_WriteLine T_string (T_object) (T_object) = 'Nothing
  rawInvokeS3 x y z = getMethodStub (tString @T_Console) (tString @T_WriteLine) (tString @T_string ++ ";" ++ tString @T_object ++ ";" ++ tString @T_object) >>= return . makeWriteLineType4 >>= \f-> f x y z

foreign import ccall "dynamic" makeWriteLineType4 :: FunPtr (BStr -> (ObjectID a) -> (ObjectID b) -> IO ()) -> (BStr -> (ObjectID a) -> (ObjectID b) -> IO ())

instance (TString t) => Constructor1 (T_List t) () where
  rawNew1 () = getMethodStub (tString @(T_List t)) ".ctor" (tString @())  >>= return . makeListCTor >>= \f-> f

foreign import ccall "dynamic" makeListCTor :: FunPtr (IO (ObjectID a)) -> IO (ObjectID a)

instance MethodI1 (T_List T_string) T_Add T_string where
  type ResultTypeI1 (T_List T_string) T_Add T_string = 'Nothing
  rawInvokeI1 l s = getMethodStub (tString @(T_List T_string)) (tString @T_Add) (tString @T_string) >>= return . makeListAdd >>= \f-> f l s

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
  let prodList = toProducer list                                                -- IEnumerable implementors can be converted to Producers (pipes package)
  runEffect $ prodList >-> stdoutLn
  return ()


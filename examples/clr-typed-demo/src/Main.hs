{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeApplications, ScopedTypeVariables #-}

module Main where

import Clr
import Clr.Resolver
import Clr.TypeString

import Clr.Host
import Clr.Host.GCHandle
import Clr.Host.Box

import Clr.Bindings
import Clr.Bindings.IEnumerable

import Data.Int(Int32, Int64)
import Foreign.Ptr(Ptr, FunPtr)
import System.IO

import Pipes
import Pipes.Prelude.Text(stdoutLn)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

type T_Console                  = T "System.Console" '[]
type T_List t                   = T "System.Collections.Generic.List" '[t]
type T_Thread                   = T "System.Threading.Thread" '[]
type T_ParameterizedThreadStart = T "System.Threading.ParameterizedThreadStart" '[]

type T_Add       = T "Add" '[]
type T_WriteLine = T "WriteLine" '[]
type T_Start     = T "Start" '[]
type T_Join      = T "Join" '[]

type instance SuperTypes (T_List t) = '[ T_IEnumerable t, T_IEnumerable', T_object ]
type instance SuperTypes (T_Thread) = '[ T_object ]

type instance Members (T_List t) = '[ T_Add ]
type instance Members  T_Thread  = '[ T_Start, T_Join ]

type instance Candidates (T_List t) T_Add      = '[ '[t] ]
type instance Candidates T_Console T_WriteLine = '[ '[                              ]
                                                  , '[ T_string                     ]
                                                  , '[ T_int                        ]
                                                  , '[ T_string, T_object           ]
                                                  , '[ T_string, T_object, T_object ] ]
type instance Candidates (T_List t) (T_List t) = '[ '[] ]
type instance Candidates (T_Thread) (T_Thread) = '[ '[ T_ParameterizedThreadStart ] ]
type instance Candidates (T_Thread) (T_Start)  = '[ '[ T_object ] ]
type instance Candidates (T_Thread) (T_Join)   = '[ '[ ] ]

foreign import ccall "dynamic" makeWriteLineType0 :: FunPtr (IO ()) -> IO ()
foreign import ccall "dynamic" makeWriteLineType1 :: FunPtr (BStr -> IO ()) -> (BStr -> IO ())
foreign import ccall "dynamic" makeWriteLineType2 :: FunPtr (Int32 -> IO ()) -> (Int32 -> IO ())
foreign import ccall "dynamic" makeWriteLineType3 :: FunPtr (BStr -> (GCHandle a) -> IO ()) -> (BStr -> (GCHandle a) -> IO ())
foreign import ccall "dynamic" makeWriteLineType4 :: FunPtr (BStr -> (GCHandle a) -> (GCHandle b) -> IO ()) -> (BStr -> (GCHandle a) -> (GCHandle b) -> IO ())
foreign import ccall "dynamic" makeListCTor :: FunPtr (IO (GCHandle a)) -> IO (GCHandle a)
foreign import ccall "dynamic" makeListAdd :: FunPtr (GCHandle a -> BStr -> IO ()) -> (GCHandle a -> BStr -> IO ())
foreign import ccall "dynamic" makeThreadParameterizedThreadStart :: FunPtr (GCHandle a -> IO (GCHandle b)) -> (GCHandle a -> IO (GCHandle b))
foreign import ccall "wrapper" wrapParameterizedThreadStart :: (GCHandle a -> IO ()) -> IO (FunPtr (GCHandle a -> IO ()))
foreign import ccall "dynamic" makeThreadStart :: FunPtr (GCHandle a -> GCHandle b -> IO ()) -> (GCHandle a -> GCHandle b -> IO ())
foreign import ccall "dynamic" makeThreadJoin :: FunPtr (GCHandle a -> IO ()) -> (GCHandle a -> IO ())

instance MethodResultS1 T_Console T_WriteLine arg0 where
  type ResultTypeS1 T_Console T_WriteLine arg0 = 'Nothing

instance MethodResultS2 T_Console T_WriteLine arg0 arg1 where
  type ResultTypeS2 T_Console T_WriteLine arg0 arg1 = 'Nothing

instance MethodResultS3 T_Console T_WriteLine arg0 arg1 arg2 where
  type ResultTypeS3 T_Console T_WriteLine arg0 arg1 arg2 = 'Nothing

instance MethodResultI1 (T_List T_string) T_Add arg0 where
  type ResultTypeI1 (T_List T_string) T_Add arg0 = 'Nothing

instance MethodResultI1 T_Thread T_Start arg0 where
  type ResultTypeI1 T_Thread T_Start arg0 = 'Nothing

instance MethodResultI1 T_Thread T_Join () where
  type ResultTypeI1 T_Thread T_Join () = 'Nothing

instance MethodDynImportS1 T_Console T_WriteLine () where
  methodDynImportS1 = makeWriteLineType0

instance MethodDynImportS1 T_Console T_WriteLine T_string where
  methodDynImportS1 = makeWriteLineType1

instance MethodDynImportS1 T_Console T_WriteLine T_int where
  methodDynImportS1 = makeWriteLineType2

instance MethodDynImportS2 T_Console T_WriteLine T_string T_object where
  methodDynImportS2 = makeWriteLineType3

instance MethodDynImportS3 T_Console T_WriteLine T_string (T_object) (T_object) where
  methodDynImportS3 = makeWriteLineType4

instance ConstructorDynImport1 (T_List t) () where
  constructorDynImport1 = makeListCTor

instance MethodDynImportI1 (T_List T_string) T_Add T_string where
  methodDynImportI1 = makeListAdd

instance ConstructorDynImport1 T_Thread T_ParameterizedThreadStart where
  constructorDynImport1 = makeThreadParameterizedThreadStart

instance WrapperImport T_ParameterizedThreadStart where
  wrapperImport = wrapParameterizedThreadStart

instance Delegate T_ParameterizedThreadStart where
  type DelegateArgTypes   T_ParameterizedThreadStart = '[ T_object ]
  type DelegateResultType T_ParameterizedThreadStart = 'Nothing

instance MethodDynImportI1 T_Thread T_Start T_object where
  methodDynImportI1 = makeThreadStart

instance MethodDynImportI1 T_Thread T_Join () where
  methodDynImportI1 = makeThreadJoin

onThreadStart :: Object T_object -> IO ()
onThreadStart obj = putStrLn "Haskell being used as a delegate and called from the CLR!"

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

  d <- delegate @T_ParameterizedThreadStart onThreadStart                       -- Compatible Haskell functions can be turned into delegates

  thread <- new @T_Thread d                                                     -- And then used as you would normally
  invokeI @"Start" thread "SomeParam"
  invokeI @"Join" thread ()

  return ()



{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeApplications, ScopedTypeVariables #-}

module Main where

import Clr
import Clr.Host
import Clr.TypeString

import Clr.Bindings
import Clr.Bindings.Host
import Clr.Bindings.IEnumerable

import Data.Int(Int32, Int64)
import Foreign.Ptr(Ptr, FunPtr)

import Pipes
import Pipes.Prelude.Text(stdoutLn)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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

foreign import ccall "dynamic" makeWriteLineType0 :: FunPtr (IO ()) -> IO ()
foreign import ccall "dynamic" makeWriteLineType1 :: FunPtr (BStr -> IO ()) -> (BStr -> IO ())
foreign import ccall "dynamic" makeWriteLineType2 :: FunPtr (Int32 -> IO ()) -> (Int32 -> IO ())
foreign import ccall "dynamic" makeWriteLineType3 :: FunPtr (BStr -> (ObjectID a) -> IO ()) -> (BStr -> (ObjectID a) -> IO ())
foreign import ccall "dynamic" makeWriteLineType4 :: FunPtr (BStr -> (ObjectID a) -> (ObjectID b) -> IO ()) -> (BStr -> (ObjectID a) -> (ObjectID b) -> IO ())
foreign import ccall "dynamic" makeListCTor :: FunPtr (IO (ObjectID a)) -> IO (ObjectID a)
foreign import ccall "dynamic" makeListAdd :: FunPtr (ObjectID a -> BStr -> IO ()) -> (ObjectID a -> BStr -> IO ())

instance MethodResultS1 T_Console T_WriteLine arg0 where
  type ResultTypeS1 T_Console T_WriteLine arg0 = 'Nothing

instance MethodResultS2 T_Console T_WriteLine arg0 arg1 where
  type ResultTypeS2 T_Console T_WriteLine arg0 arg1 = 'Nothing

instance MethodResultS3 T_Console T_WriteLine arg0 arg1 arg2 where
  type ResultTypeS3 T_Console T_WriteLine arg0 arg1 arg2 = 'Nothing

instance MethodResultI1 (T_List T_string) T_Add arg0 where
  type ResultTypeI1 (T_List T_string) T_Add arg0 = 'Nothing

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


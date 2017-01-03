{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeApplications #-}

module Main where

import Clr
import Clr.Host
import Clr.Bindings
import Clr.Resolver
import Clr.Marshal
import Clr.Object

import Clr.InstanceMethod
import Clr.StaticMethod
import Clr.Constructor

import Data.Int(Int32, Int64)
import Data.Word(Word16)
import Foreign.Ptr(Ptr, FunPtr)


instance MethodS1 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) where
  type ResultTypeS1 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) = 'Nothing
  rawInvokeS1 x = getMethodStub "System.Console, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "WriteLine" "System.String" >>= return . makeWriteLineType1 >>= \f-> f x

foreign import ccall "dynamic" makeWriteLineType1 :: FunPtr (Ptr Word16 -> IO ()) -> (Ptr Word16 -> IO ())

instance MethodS1 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.Int32" 'Nothing '[]) where
  type ResultTypeS1 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.Int32" 'Nothing '[]) = 'Nothing
  rawInvokeS1 x = getMethodStub "System.Console, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "WriteLine" "System.Int32" >>= return . makeWriteLineType2 >>= \f-> f x

foreign import ccall "dynamic" makeWriteLineType2 :: FunPtr (Int32 -> IO ()) -> (Int32 -> IO ())

instance MethodS2 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) (T "System.Object" 'Nothing '[]) where
  type ResultTypeS2 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) (T "System.Object" 'Nothing '[]) = 'Nothing
  rawInvokeS2 x y = getMethodStub "System.Console, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "WriteLine" "System.String;System.Object" >>= return . makeWriteLineType3 >>= \f-> f x y

foreign import ccall "dynamic" makeWriteLineType3 :: FunPtr (Ptr Word16 -> (ObjectID a) -> IO ()) -> (Ptr Word16 -> (ObjectID a) -> IO ())

instance MethodS3 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) (T "System.Object" 'Nothing '[]) (T "System.Object" 'Nothing '[]) where
  type ResultTypeS3 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) (T "System.Object" 'Nothing '[]) (T "System.Object" 'Nothing '[]) = 'Nothing
  rawInvokeS3 x y z = getMethodStub "System.Console, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089" "WriteLine" "System.String;System.Object;System.Object" >>= return . makeWriteLineType4 >>= \f-> f x y z

foreign import ccall "dynamic" makeWriteLineType4 :: FunPtr (Ptr Word16 -> (ObjectID a) -> (ObjectID b) -> IO ()) -> (Ptr Word16 -> (ObjectID a) -> (ObjectID b) -> IO ())



type instance Candidates (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) = '[ '[ T "System.String" 'Nothing '[] ]
                                                                                           , '[ T "System.Int32" 'Nothing '[] ]
                                                                                           , '[ T "System.String" 'Nothing '[], T "System.Object" 'Nothing '[] ]
                                                                                           , '[ T "System.String" 'Nothing '[], T "System.Object" 'Nothing '[], T "System.Object" 'Nothing '[] ] ]
main :: IO ()
main = do
  startClr
  invokeS @"WriteLine" @"System.Console" "Hello CLR!!!"                         -- Console.WriteLine(String)
  invokeS @"WriteLine" @"System.Console" (2 :: Int32)                           -- Console.WriteLine(Int32)
  invokeS @"WriteLine" @"System.Console" ("The year is {0}", 2017::Int32)       -- Console.WriteLine(String, Object)
  invokeS @"WriteLine" @"System.Console" ("Well {0} {1}", "This", "Is Cool")    -- Console.WriteLine(String, Object, Object)

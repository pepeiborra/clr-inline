{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}

module Bindings where

import Clr
import Foreign.C
import Data.Int
import Data.Kind

--
-- Static method
--
writeLineRaw1 :: CString -> IO ()
writeLineRaw1 cs = putStrLn "Console.WriteLine(String)"
writeLineRaw2 :: CString -> CString -> IO ()
writeLineRaw2 cs1 cs2 = putStrLn "Console.WriteLine(String, String)"

instance MethodS1 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) where
  type ResultTypeS1 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) = 'Nothing
  rawInvokeS1 = writeLineRaw1

instance MethodS2 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) (T "System.String" '[]) where
  type ResultTypeS2 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) (T "System.String" '[]) = 'Nothing
  rawInvokeS2 = writeLineRaw2

--
-- Base type
--

type instance SuperTypeOf (T "BaseType" '[]) = 'Just (T "System.Object" '[])

instance Constructor1 (T "BaseType" '[]) () where
  rawNew1 () = putStrLn "Constructed BaseType" >> return (1::Int64)

type instance Members (T "BaseType" '[]) = '[(T "Foo" '[]), (T "Bar" '[])]

-- Foo
instance MethodI1 (T "BaseType" '[]) (T "Foo" '[]) (T "System.String" '[]) where
  type ResultTypeI1 (T "BaseType" '[]) (T "Foo" '[]) (T "System.String" '[]) = 'Nothing
  rawInvokeI1 = rawInvokeBaseTypeFooStr

instance MethodI1 (T "BaseType" '[]) (T "Foo" '[]) (T "System.Int64" '[]) where
  type ResultTypeI1 (T "BaseType" '[]) (T "Foo" '[]) (T "System.Int64" '[]) = 'Nothing
  rawInvokeI1 = rawInvokeBaseTypeFooInt64

instance MethodI1 (T "BaseType" '[]) (T "Foo" '[]) (T "System.Int32" '[]) where
  type ResultTypeI1 (T "BaseType" '[]) (T "Foo" '[]) (T "System.Int32" '[]) = 'Nothing
  rawInvokeI1 = rawInvokeBaseTypeFooInt32

rawInvokeBaseTypeFooStr :: ObjectID -> CString -> IO ()
rawInvokeBaseTypeFooStr d s = putStrLn "BaseType.Foo(String)"

rawInvokeBaseTypeFooInt64 :: ObjectID -> Int64 -> IO ()
rawInvokeBaseTypeFooInt64 d s = putStrLn "BaseType.Foo(Int64)"

rawInvokeBaseTypeFooInt32 :: ObjectID -> Int32 -> IO ()
rawInvokeBaseTypeFooInt32 d s = putStrLn "BaseType.Foo(Int32)"

-- Bar
instance MethodI1 (T "BaseType" '[]) (T "Bar" '[]) (T "System.String" '[]) where
  type ResultTypeI1 (T "BaseType" '[]) (T "Bar" '[]) (T "System.String" '[]) = 'Nothing
  rawInvokeI1 = rawInvokeBaseTypeBarStr

instance MethodI1 (T "BaseType" '[]) (T "Bar" '[]) (T "System.Int64" '[]) where
  type ResultTypeI1 (T "BaseType" '[]) (T "Bar" '[]) (T "System.Int64" '[]) = 'Nothing
  rawInvokeI1 = rawInvokeBaseTypeBarInt64

instance MethodI1 (T "BaseType" '[]) (T "Bar" '[]) (T "System.Int32" '[]) where
  type ResultTypeI1 (T "BaseType" '[]) (T "Bar" '[]) (T "System.Int32" '[]) = 'Nothing
  rawInvokeI1 = rawInvokeBaseTypeBarInt32

rawInvokeBaseTypeBarStr :: ObjectID -> CString -> IO ()
rawInvokeBaseTypeBarStr d s = putStrLn "BaseType.Bar(String)"

rawInvokeBaseTypeBarInt64 :: ObjectID -> Int64 -> IO ()
rawInvokeBaseTypeBarInt64 d s = putStrLn "BaseType.Bar(Int64)"

rawInvokeBaseTypeBarInt32 :: ObjectID -> Int32 -> IO ()
rawInvokeBaseTypeBarInt32 d s = putStrLn "BaseType.Bar(Int32)"

--
-- Derived type
--

type instance SuperTypeOf (T "DerivedType" '[]) = 'Just (T "BaseType" '[])

instance Constructor1 (T "DerivedType" '[]) () where
  rawNew1 () = putStrLn "Constructed DerivedType" >> return (1::Int64)

type instance Members (T "DerivedType" '[]) = '[(T "Foo" '[])]

instance MethodI1 (T "DerivedType" '[]) (T "Foo" '[]) (T "System.String" '[]) where
  type ResultTypeI1 (T "DerivedType" '[]) (T "Foo" '[]) (T "System.String" '[]) = 'Nothing
  rawInvokeI1 = rawInvokeDerivedTypeStr

instance MethodI1 (T "DerivedType" '[]) (T "Foo" '[]) (T "System.Int64" '[]) where
  type ResultTypeI1 (T "DerivedType" '[]) (T "Foo" '[]) (T "System.Int64" '[]) = 'Nothing
  rawInvokeI1 = rawInvokeDerivedTypeInt64

instance MethodI1 (T "DerivedType" '[]) (T "Foo" '[]) (T "System.Int32" '[]) where
  type ResultTypeI1 (T "DerivedType" '[]) (T "Foo" '[]) (T "System.Int32" '[]) = 'Nothing
  rawInvokeI1 = rawInvokeDerivedTypeInt32

rawInvokeDerivedTypeStr :: ObjectID -> CString -> IO ()
rawInvokeDerivedTypeStr d s = putStrLn "DerivedType.Foo(String)"

rawInvokeDerivedTypeInt64 :: ObjectID -> Int64 -> IO ()
rawInvokeDerivedTypeInt64 d s = putStrLn "DerivedType.Foo(Int64)"

rawInvokeDerivedTypeInt32 :: ObjectID -> Int32 -> IO ()
rawInvokeDerivedTypeInt32 d s = putStrLn "DerivedType.Foo(Int32)"

type instance SuperTypeOf (T "MyGenType" '[gt0]) = 'Just (T "System.Object" '[])

instance Constructor1 (T "MyGenType" '[gt0]) () where
  rawNew1 () = putStrLn "Constructed MyGenType" >> return (1::Int64)

type instance Members (T "MyGenType" '[gt0]) = '[(T "Add" '[])]

instance MethodI1 (T "MyGenType" '[(T "System.String" '[])]) (T "Add" '[]) (T "System.String" '[]) where
  type ResultTypeI1 (T "MyGenType" '[(T "System.String" '[])]) (T "Add" '[]) (T "System.String" '[]) = 'Nothing
  rawInvokeI1 = rawInvokeMyGenTypeAddStr

instance MethodI1 (T "MyGenType" '[(T "System.Int32" '[])]) (T "Add" '[]) (T "System.Int32" '[]) where
  type ResultTypeI1 (T "MyGenType" '[(T "System.Int32" '[])]) (T "Add" '[]) (T "System.Int32" '[]) = 'Nothing
  rawInvokeI1 = rawInvokeMyGenTypeAddInt


rawInvokeMyGenTypeAddStr :: ObjectID -> CString -> IO ()
rawInvokeMyGenTypeAddStr oid s = putStrLn "MyGenType.Add(String)"

rawInvokeMyGenTypeAddInt :: ObjectID -> Int32 -> IO ()
rawInvokeMyGenTypeAddInt oid s = putStrLn "MyGenType.Add(Int32)"

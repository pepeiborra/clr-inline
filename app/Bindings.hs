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

instance MethodS (ClrType "System.Console" '[]) (ClrType "WriteLine" '[]) '[(ClrType "System.String" '[])] where
  type ResultTypeS (ClrType "System.Console" '[]) (ClrType "WriteLine" '[]) '[(ClrType "System.String" '[])] = 'Nothing
  rawInvokeS = writeLineRaw1

instance MethodS (ClrType "System.Console" '[]) (ClrType "WriteLine" '[]) '[(ClrType "System.String" '[]), (ClrType "System.String" '[])] where
  type ResultTypeS (ClrType "System.Console" '[]) (ClrType "WriteLine" '[]) '[(ClrType "System.String" '[]), (ClrType "System.String" '[])] = 'Nothing
  rawInvokeS = writeLineRaw2

--
-- Base type
--

type instance SuperTypeOf (ClrType "BaseType" '[]) = 'Just (ClrType "System.Object" '[])

instance Constructor (ClrType "BaseType" '[]) '[] where
  rawNew () = putStrLn "Constructed BaseType" >> return (1::Int64)

type instance Members (ClrType "BaseType" '[]) = '[(ClrType "Foo" '[]), (ClrType "Bar" '[])]

-- Foo
instance MethodI (ClrType "BaseType" '[]) (ClrType "Foo" '[]) '[(ClrType "System.String" '[])] where
  type ResultTypeI (ClrType "BaseType" '[]) (ClrType "Foo" '[]) '[(ClrType "System.String" '[])] = 'Nothing
  rawInvokeI = rawInvokeBaseTypeFooStr

instance MethodI (ClrType "BaseType" '[]) (ClrType "Foo" '[]) '[(ClrType "System.Int64" '[])] where
  type ResultTypeI (ClrType "BaseType" '[]) (ClrType "Foo" '[]) '[(ClrType "System.Int64" '[])] = 'Nothing
  rawInvokeI = rawInvokeBaseTypeFooInt64

instance MethodI (ClrType "BaseType" '[]) (ClrType "Foo" '[]) '[(ClrType "System.Int32" '[])] where
  type ResultTypeI (ClrType "BaseType" '[]) (ClrType "Foo" '[]) '[(ClrType "System.Int32" '[])] = 'Nothing
  rawInvokeI = rawInvokeBaseTypeFooInt32

rawInvokeBaseTypeFooStr :: ObjectID -> CString -> IO ()
rawInvokeBaseTypeFooStr d s = putStrLn "BaseType.Foo(String)"

rawInvokeBaseTypeFooInt64 :: ObjectID -> Int64 -> IO ()
rawInvokeBaseTypeFooInt64 d s = putStrLn "BaseType.Foo(Int64)"

rawInvokeBaseTypeFooInt32 :: ObjectID -> Int32 -> IO ()
rawInvokeBaseTypeFooInt32 d s = putStrLn "BaseType.Foo(Int32)"

-- Bar
instance MethodI (ClrType "BaseType" '[]) (ClrType "Bar" '[]) '[(ClrType "System.String" '[])] where
  type ResultTypeI (ClrType "BaseType" '[]) (ClrType "Bar" '[]) '[(ClrType "System.String" '[])] = 'Nothing
  rawInvokeI = rawInvokeBaseTypeBarStr

instance MethodI (ClrType "BaseType" '[]) (ClrType "Bar" '[]) '[(ClrType "System.Int64" '[])] where
  type ResultTypeI (ClrType "BaseType" '[]) (ClrType "Bar" '[]) '[(ClrType "System.Int64" '[])] = 'Nothing
  rawInvokeI = rawInvokeBaseTypeBarInt64

instance MethodI (ClrType "BaseType" '[]) (ClrType "Bar" '[]) '[(ClrType "System.Int32" '[])] where
  type ResultTypeI (ClrType "BaseType" '[]) (ClrType "Bar" '[]) '[(ClrType "System.Int32" '[])] = 'Nothing
  rawInvokeI = rawInvokeBaseTypeBarInt32

rawInvokeBaseTypeBarStr :: ObjectID -> CString -> IO ()
rawInvokeBaseTypeBarStr d s = putStrLn "BaseType.Bar(String)"

rawInvokeBaseTypeBarInt64 :: ObjectID -> Int64 -> IO ()
rawInvokeBaseTypeBarInt64 d s = putStrLn "BaseType.Bar(Int64)"

rawInvokeBaseTypeBarInt32 :: ObjectID -> Int32 -> IO ()
rawInvokeBaseTypeBarInt32 d s = putStrLn "BaseType.Bar(Int32)"

--
-- Derived type
--

type instance SuperTypeOf (ClrType "DerivedType" '[]) = 'Just (ClrType "BaseType" '[])

instance Constructor (ClrType "DerivedType" '[]) '[] where
  rawNew () = putStrLn "Constructed DerivedType" >> return (1::Int64)

type instance Members (ClrType "DerivedType" '[]) = '[(ClrType "Foo" '[])]

instance MethodI (ClrType "DerivedType" '[]) (ClrType "Foo" '[]) '[(ClrType "System.String" '[])] where
  type ResultTypeI (ClrType "DerivedType" '[]) (ClrType "Foo" '[]) '[(ClrType "System.String" '[])] = 'Nothing
  rawInvokeI = rawInvokeDerivedTypeStr

instance MethodI (ClrType "DerivedType" '[]) (ClrType "Foo" '[]) '[(ClrType "System.Int64" '[])] where
  type ResultTypeI (ClrType "DerivedType" '[]) (ClrType "Foo" '[]) '[(ClrType "System.Int64" '[])] = 'Nothing
  rawInvokeI = rawInvokeDerivedTypeInt64

instance MethodI (ClrType "DerivedType" '[]) (ClrType "Foo" '[]) '[(ClrType "System.Int32" '[])] where
  type ResultTypeI (ClrType "DerivedType" '[]) (ClrType "Foo" '[]) '[(ClrType "System.Int32" '[])] = 'Nothing
  rawInvokeI = rawInvokeDerivedTypeInt32

rawInvokeDerivedTypeStr :: ObjectID -> CString -> IO ()
rawInvokeDerivedTypeStr d s = putStrLn "DerivedType.Foo(String)"

rawInvokeDerivedTypeInt64 :: ObjectID -> Int64 -> IO ()
rawInvokeDerivedTypeInt64 d s = putStrLn "DerivedType.Foo(Int64)"

rawInvokeDerivedTypeInt32 :: ObjectID -> Int32 -> IO ()
rawInvokeDerivedTypeInt32 d s = putStrLn "DerivedType.Foo(Int32)"

type instance SuperTypeOf (ClrType "MyGenType" (gt0 ': '[])) = 'Just (ClrType "System.Object" '[])

instance Constructor (ClrType "MyGenType" (gt0 ': '[])) '[] where
  rawNew () = putStrLn "Constructed MyGenType" >> return (1::Int64)

type instance Members (ClrType "MyGenType" (gt0 ': '[])) = '[(ClrType "Add" '[])]

instance MethodI1 (ClrType "MyGenType" '[gt0]) (ClrType "Add" '[]) gt0 where
  type ResultTypeI1 (ClrType "MyGenType" (gt0 ': '[])) (ClrType "Add" '[]) gt0 = 'Nothing
  rawInvokeI1 = rawInvokeMyGenTypeAdd

rawInvokeMyGenTypeAdd :: ObjectID -> t -> IO ()
rawInvokeMyGenTypeAdd oid t = putStrLn "MyGenType.Add(t)"


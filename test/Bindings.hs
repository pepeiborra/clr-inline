{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}

module Bindings where

import Clr
import Clr.Resolver
import Foreign.C
import Data.Int

--
-- Static method
--
writeLineRaw1 :: CString -> IO ()
writeLineRaw1 cs = putStrLn "Console.WriteLine(String)"
writeLineRaw2 :: CString -> CString -> IO ()
writeLineRaw2 cs1 cs2 = putStrLn "Console.WriteLine(String, String)"

instance MethodS1 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) where
  type ResultTypeS1 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) = 'Nothing
  rawInvokeS1 = writeLineRaw1

instance MethodS2 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) (T "System.String" 'Nothing '[]) where
  type ResultTypeS2 (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) (T "System.String" 'Nothing '[]) (T "System.String" 'Nothing '[]) = 'Nothing
  rawInvokeS2 = writeLineRaw2

type instance Candidates (T "System.Console" 'Nothing '[]) (T "WriteLine" 'Nothing '[]) = '[ '[ T "System.String" 'Nothing '[]], '[T "System.String" 'Nothing '[], T "System.String" 'Nothing '[]]]

--
-- Base type
--

type instance SuperTypeOf (T "BaseType" 'Nothing '[]) = 'Just (T "System.Object" 'Nothing '[])

instance Constructor1 (T "BaseType" 'Nothing '[]) () where
  rawNew1 () = putStrLn "Constructed BaseType" >> return (1::Int64)

type instance Candidates (T "BaseType" 'Nothing '[]) (T "BaseType" 'Nothing '[]) = '[ '[] ]
type instance Candidates (T "BaseType" 'Nothing '[]) (T "Foo" 'Nothing '[]) = '[ '[ T "System.String" 'Nothing '[] ], '[ T "System.Int64" 'Nothing '[] ], '[ T "System.Int32" 'Nothing '[] ]]
type instance Candidates (T "BaseType" 'Nothing '[]) (T "Bar" 'Nothing '[]) = '[ '[ T "System.String" 'Nothing '[] ], '[ T "System.Int64" 'Nothing '[] ], '[ T "System.Int32" 'Nothing '[] ]]

type instance Members (T "BaseType" 'Nothing '[]) = '[(T "Foo" 'Nothing '[]), (T "Bar" 'Nothing '[])]

-- Foo
instance MethodI1 (T "BaseType" 'Nothing '[]) (T "Foo" 'Nothing '[]) (T "System.String" 'Nothing '[]) where
  type ResultTypeI1 (T "BaseType" 'Nothing '[]) (T "Foo" 'Nothing '[]) (T "System.String" 'Nothing '[]) = 'Nothing
  rawInvokeI1 = rawInvokeBaseTypeFooStr

instance MethodI1 (T "BaseType" 'Nothing '[]) (T "Foo" 'Nothing '[]) (T "System.Int64" 'Nothing '[]) where
  type ResultTypeI1 (T "BaseType" 'Nothing '[]) (T "Foo" 'Nothing '[]) (T "System.Int64" 'Nothing '[]) = 'Nothing
  rawInvokeI1 = rawInvokeBaseTypeFooInt64

instance MethodI1 (T "BaseType" 'Nothing '[]) (T "Foo" 'Nothing '[]) (T "System.Int32" 'Nothing '[]) where
  type ResultTypeI1 (T "BaseType" 'Nothing '[]) (T "Foo" 'Nothing '[]) (T "System.Int32" 'Nothing '[]) = 'Nothing
  rawInvokeI1 = rawInvokeBaseTypeFooInt32

rawInvokeBaseTypeFooStr :: ObjectID -> CString -> IO ()
rawInvokeBaseTypeFooStr d s = putStrLn "BaseType.Foo(String)"

rawInvokeBaseTypeFooInt64 :: ObjectID -> Int64 -> IO ()
rawInvokeBaseTypeFooInt64 d s = putStrLn "BaseType.Foo(Int64)"

rawInvokeBaseTypeFooInt32 :: ObjectID -> Int32 -> IO ()
rawInvokeBaseTypeFooInt32 d s = putStrLn "BaseType.Foo(Int32)"

-- Bar
instance MethodI1 (T "BaseType" 'Nothing '[]) (T "Bar" 'Nothing '[]) (T "System.String" 'Nothing '[]) where
  type ResultTypeI1 (T "BaseType" 'Nothing '[]) (T "Bar" 'Nothing '[]) (T "System.String" 'Nothing '[]) = 'Nothing
  rawInvokeI1 = rawInvokeBaseTypeBarStr

instance MethodI1 (T "BaseType" 'Nothing '[]) (T "Bar" 'Nothing '[]) (T "System.Int64" 'Nothing '[]) where
  type ResultTypeI1 (T "BaseType" 'Nothing '[]) (T "Bar" 'Nothing '[]) (T "System.Int64" 'Nothing '[]) = 'Nothing
  rawInvokeI1 = rawInvokeBaseTypeBarInt64

instance MethodI1 (T "BaseType" 'Nothing '[]) (T "Bar" 'Nothing '[]) (T "System.Int32" 'Nothing '[]) where
  type ResultTypeI1 (T "BaseType" 'Nothing '[]) (T "Bar" 'Nothing '[]) (T "System.Int32" 'Nothing '[]) = 'Nothing
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

type instance SuperTypeOf (T "DerivedType" 'Nothing '[]) = 'Just (T "BaseType" 'Nothing '[])

instance Constructor1 (T "DerivedType" 'Nothing '[]) () where
  rawNew1 () = putStrLn "Constructed DerivedType" >> return (1::Int64)

type instance Candidates (T "DerivedType" 'Nothing '[]) (T "DerivedType" 'Nothing '[]) = '[ '[] ]
type instance Candidates (T "DerivedType" 'Nothing '[]) (T "Foo" 'Nothing '[]) = '[ '[ T "System.String" 'Nothing '[] ], '[ T "System.Int64" 'Nothing '[] ], '[ T "System.Int32" 'Nothing '[] ]]

type instance Members (T "DerivedType" 'Nothing '[]) = '[(T "Foo" 'Nothing '[])]

instance MethodI1 (T "DerivedType" 'Nothing '[]) (T "Foo" 'Nothing '[]) (T "System.String" 'Nothing '[]) where
  type ResultTypeI1 (T "DerivedType" 'Nothing '[]) (T "Foo" 'Nothing '[]) (T "System.String" 'Nothing '[]) = 'Nothing
  rawInvokeI1 = rawInvokeDerivedTypeStr

instance MethodI1 (T "DerivedType" 'Nothing '[]) (T "Foo" 'Nothing '[]) (T "System.Int64" 'Nothing '[]) where
  type ResultTypeI1 (T "DerivedType" 'Nothing '[]) (T "Foo" 'Nothing '[]) (T "System.Int64" 'Nothing '[]) = 'Nothing
  rawInvokeI1 = rawInvokeDerivedTypeInt64

instance MethodI1 (T "DerivedType" 'Nothing '[]) (T "Foo" 'Nothing '[]) (T "System.Int32" 'Nothing '[]) where
  type ResultTypeI1 (T "DerivedType" 'Nothing '[]) (T "Foo" 'Nothing '[]) (T "System.Int32" 'Nothing '[]) = 'Nothing
  rawInvokeI1 = rawInvokeDerivedTypeInt32

rawInvokeDerivedTypeStr :: ObjectID -> CString -> IO ()
rawInvokeDerivedTypeStr d s = putStrLn "DerivedType.Foo(String)"

rawInvokeDerivedTypeInt64 :: ObjectID -> Int64 -> IO ()
rawInvokeDerivedTypeInt64 d s = putStrLn "DerivedType.Foo(Int64)"

rawInvokeDerivedTypeInt32 :: ObjectID -> Int32 -> IO ()
rawInvokeDerivedTypeInt32 d s = putStrLn "DerivedType.Foo(Int32)"

type instance SuperTypeOf (T "MyGenType" 'Nothing '[gt0]) = 'Just (T "System.Object" 'Nothing '[])

type instance Interfaces (T "MyGenType" 'Nothing t) = '[T "IEnumerable" 'Nothing '[], T "IEnumerable" 'Nothing t]

instance Constructor1 (T "MyGenType" 'Nothing '[gt0]) () where
  rawNew1 () = putStrLn "Constructed MyGenType" >> return (1::Int64)

type instance Candidates (T "MyGenType" 'Nothing '[gt0]) (T "MyGenType" 'Nothing '[gt0]) = '[ '[] ]
type instance Candidates (T "MyGenType" 'Nothing '[gt0]) (T "Add" 'Nothing '[]) = '[ '[ gt0 ] ]

type instance Members (T "MyGenType" 'Nothing '[gt0]) = '[(T "Add" 'Nothing '[])]

instance MethodI1 (T "MyGenType" 'Nothing '[(T "System.String" 'Nothing '[])]) (T "Add" 'Nothing '[]) (T "System.String" 'Nothing '[]) where
  type ResultTypeI1 (T "MyGenType" 'Nothing '[(T "System.String" 'Nothing '[])]) (T "Add" 'Nothing '[]) (T "System.String" 'Nothing '[]) = 'Nothing
  rawInvokeI1 = rawInvokeMyGenTypeAddStr

instance MethodI1 (T "MyGenType" 'Nothing '[(T "System.Int32" 'Nothing '[])]) (T "Add" 'Nothing '[]) (T "System.Int32" 'Nothing '[]) where
  type ResultTypeI1 (T "MyGenType" 'Nothing '[(T "System.Int32" 'Nothing '[])]) (T "Add" 'Nothing '[]) (T "System.Int32" 'Nothing '[]) = 'Nothing
  rawInvokeI1 = rawInvokeMyGenTypeAddInt

rawInvokeMyGenTypeAddStr :: ObjectID -> CString -> IO ()
rawInvokeMyGenTypeAddStr oid s = putStrLn "MyGenType.Add(String)"

rawInvokeMyGenTypeAddInt :: ObjectID -> Int32 -> IO ()
rawInvokeMyGenTypeAddInt oid s = putStrLn "MyGenType.Add(Int32)"

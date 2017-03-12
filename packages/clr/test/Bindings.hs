{-# LANGUAGE TypeInType, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, AllowAmbiguousTypes, ScopedTypeVariables, TypeApplications, TypeOperators #-}

module Bindings where

import Clr
import Clr.Bridge
import Data.Int
import Data.Word
import Foreign.Ptr

--
-- Just for testing
--
type instance BridgeTypePrim (T "System.String"  '[]) = String

--
-- Static method
--
writeLineRaw1 :: String -> IO ()
writeLineRaw1 cs = putStrLn "Console.WriteLine(String)"
writeLineRaw2 :: String -> String -> IO ()
writeLineRaw2 cs1 cs2 = putStrLn "Console.WriteLine(String, String)"

instance MethodS1 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) where
  type ResultTypeS1 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) = 'Nothing
  rawInvokeS1 = writeLineRaw1

instance MethodS2 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) (T "System.String" '[]) where
  type ResultTypeS2 (T "System.Console" '[]) (T "WriteLine" '[]) (T "System.String" '[]) (T "System.String" '[]) = 'Nothing
  rawInvokeS2 = writeLineRaw2

type instance Candidates (T "System.Console" '[]) (T "WriteLine" '[]) = '[ '[ T "System.String" '[]], '[T "System.String" '[], T "System.String" '[]]]

--
-- Base type
--

type instance SuperTypes (T "BaseType" '[]) = '[ (T "System.Object" '[]) ]

instance Constructor1 (T "BaseType" '[]) () where
  rawNew1 () = putStrLn "Constructed BaseType" >> return (ObjectID 1)

type instance Candidates (T "BaseType" '[]) (T "BaseType" '[]) = '[ '[] ]
type instance Candidates (T "BaseType" '[]) (T "Foo" '[]) = '[ '[ T "System.String" '[] ], '[ T "System.Int64" '[] ], '[ T "System.Int32" '[] ]]
type instance Candidates (T "BaseType" '[]) (T "Bar" '[]) = '[ '[ T "System.String" '[] ], '[ T "System.Int64" '[] ], '[ T "System.Int32" '[] ]]

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

rawInvokeBaseTypeFooStr :: ObjectID t -> String -> IO ()
rawInvokeBaseTypeFooStr d s = putStrLn "BaseType.Foo(String)"

rawInvokeBaseTypeFooInt64 :: ObjectID t -> Int64 -> IO ()
rawInvokeBaseTypeFooInt64 d s = putStrLn "BaseType.Foo(Int64)"

rawInvokeBaseTypeFooInt32 :: ObjectID t -> Int32 -> IO ()
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

rawInvokeBaseTypeBarStr :: ObjectID t -> String -> IO ()
rawInvokeBaseTypeBarStr d s = putStrLn "BaseType.Bar(String)"

rawInvokeBaseTypeBarInt64 :: ObjectID t -> Int64 -> IO ()
rawInvokeBaseTypeBarInt64 d s = putStrLn "BaseType.Bar(Int64)"

rawInvokeBaseTypeBarInt32 :: ObjectID t -> Int32 -> IO ()
rawInvokeBaseTypeBarInt32 d s = putStrLn "BaseType.Bar(Int32)"

--
-- Derived type
--

type instance SuperTypes (T "DerivedType" '[]) = '[ (T "BaseType" '[]) ]

instance Constructor1 (T "DerivedType" '[]) () where
  rawNew1 () = putStrLn "Constructed DerivedType" >> return (ObjectID 1)

type instance Candidates (T "DerivedType" '[]) (T "DerivedType" '[]) = '[ '[] ]
type instance Candidates (T "DerivedType" '[]) (T "Foo" '[]) = '[ '[ T "System.String" '[] ], '[ T "System.Int64" '[] ], '[ T "System.Int32" '[] ]]

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

rawInvokeDerivedTypeStr :: ObjectID t -> String -> IO ()
rawInvokeDerivedTypeStr d s = putStrLn "DerivedType.Foo(String)"

rawInvokeDerivedTypeInt64 :: ObjectID t -> Int64 -> IO ()
rawInvokeDerivedTypeInt64 d s = putStrLn "DerivedType.Foo(Int64)"

rawInvokeDerivedTypeInt32 :: ObjectID t -> Int32 -> IO ()
rawInvokeDerivedTypeInt32 d s = putStrLn "DerivedType.Foo(Int32)"

type instance SuperTypes (T "MyGenType" '[gt0]) = '[ T "System.Object" '[], T "IEnumerable" '[], T "IEnumerable" '[gt0] ]

instance Constructor1 (T "MyGenType" '[gt0]) () where
  rawNew1 () = putStrLn "Constructed MyGenType" >> return (ObjectID 1)

type instance Candidates (T "MyGenType" '[gt0]) (T "MyGenType" '[gt0]) = '[ '[] ]
type instance Candidates (T "MyGenType" '[gt0]) (T "Add" '[]) = '[ '[ gt0 ] ]

type instance Members (T "MyGenType" '[gt0]) = '[(T "Add" '[])]

instance MethodI1 (T "MyGenType" '[(T "System.String" '[])]) (T "Add" '[]) (T "System.String" '[]) where
  type ResultTypeI1 (T "MyGenType" '[(T "System.String" '[])]) (T "Add" '[]) (T "System.String" '[]) = 'Nothing
  rawInvokeI1 = rawInvokeMyGenTypeAddStr

instance MethodI1 (T "MyGenType" '[(T "System.Int32" '[])]) (T "Add" '[]) (T "System.Int32" '[]) where
  type ResultTypeI1 (T "MyGenType" '[(T "System.Int32" '[])]) (T "Add" '[]) (T "System.Int32" '[]) = 'Nothing
  rawInvokeI1 = rawInvokeMyGenTypeAddInt

rawInvokeMyGenTypeAddStr :: ObjectID t -> String -> IO ()
rawInvokeMyGenTypeAddStr oid s = putStrLn "MyGenType.Add(String)"

rawInvokeMyGenTypeAddInt :: ObjectID t -> Int32 -> IO ()
rawInvokeMyGenTypeAddInt oid s = putStrLn "MyGenType.Add(Int32)"

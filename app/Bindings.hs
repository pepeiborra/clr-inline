{-# LANGUAGE DataKinds, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Bindings where

import Clr

import Foreign.C
import Data.Int

--
-- Static method
--
writeLineRaw1 :: CString -> IO ()
writeLineRaw1 cs = putStrLn "static method called with 1 string"
writeLineRaw2 :: CString -> CString -> IO ()
writeLineRaw2 cs1 cs2 = putStrLn "static method called with 2 strings"

instance MethodS "System.Console" "WriteLine" '["System.String"] where
  type ResultTypeS "System.Console" "WriteLine" '["System.String"] = 'Nothing
  rawInvokeS = writeLineRaw1

instance MethodS "System.Console" "WriteLine" '["System.String", "System.String"] where
  type ResultTypeS "System.Console" "WriteLine" '["System.String", "System.String"] = 'Nothing
  rawInvokeS = writeLineRaw2

--
-- Base type
--

type instance SuperTypeOf "BaseType" = 'Just "System.Object"

type instance Members "BaseType" = '["Foo", "Bar"]

-- Foo
instance MethodI "BaseType" "Foo" '["System.String"] where
  type ResultTypeI "BaseType" "Foo" '["System.String"] = 'Nothing
  rawInvokeI = rawInvokeBaseTypeFooStr

instance MethodI "BaseType" "Foo" '["System.Int64"] where
  type ResultTypeI "BaseType" "Foo" '["System.Int64"] = 'Nothing
  rawInvokeI = rawInvokeBaseTypeFooInt64

instance MethodI "BaseType" "Foo" '["System.Int32"] where
  type ResultTypeI "BaseType" "Foo" '["System.Int32"] = 'Nothing
  rawInvokeI = rawInvokeBaseTypeFooInt32

rawInvokeBaseTypeFooStr :: ObjectID -> CString -> IO ()
rawInvokeBaseTypeFooStr d s = putStrLn "BaseType.Foo(String)"

rawInvokeBaseTypeFooInt64 :: ObjectID -> Int64 -> IO ()
rawInvokeBaseTypeFooInt64 d s = putStrLn "BaseType.Foo(Int64)"

rawInvokeBaseTypeFooInt32 :: ObjectID -> Int32 -> IO ()
rawInvokeBaseTypeFooInt32 d s = putStrLn "BaseType.Foo(Int32)"

-- Bar
instance MethodI "BaseType" "Bar" '["System.String"] where
  type ResultTypeI "BaseType" "Bar" '["System.String"] = 'Nothing
  rawInvokeI = rawInvokeBaseTypeBarStr

instance MethodI "BaseType" "Bar" '["System.Int64"] where
  type ResultTypeI "BaseType" "Bar" '["System.Int64"] = 'Nothing
  rawInvokeI = rawInvokeBaseTypeBarInt64

instance MethodI "BaseType" "Bar" '["System.Int32"] where
  type ResultTypeI "BaseType" "Bar" '["System.Int32"] = 'Nothing
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

type instance SuperTypeOf "DerivedType" = 'Just "BaseType"

type instance Members "DerivedType" = '["Foo"]

instance MethodI "DerivedType" "Foo" '["System.String"] where
  type ResultTypeI "DerivedType" "Foo" '["System.String"] = 'Nothing
  rawInvokeI = rawInvokeDerivedTypeStr

instance MethodI "DerivedType" "Foo" '["System.Int64"] where
  type ResultTypeI "DerivedType" "Foo" '["System.Int64"] = 'Nothing
  rawInvokeI = rawInvokeDerivedTypeInt64

instance MethodI "DerivedType" "Foo" '["System.Int32"] where
  type ResultTypeI "DerivedType" "Foo" '["System.Int32"] = 'Nothing
  rawInvokeI = rawInvokeDerivedTypeInt32

rawInvokeDerivedTypeStr :: ObjectID -> CString -> IO ()
rawInvokeDerivedTypeStr d s = putStrLn "DerivedType.Foo(String)"

rawInvokeDerivedTypeInt64 :: ObjectID -> Int64 -> IO ()
rawInvokeDerivedTypeInt64 d s = putStrLn "DerivedType.Foo(Int64)"

rawInvokeDerivedTypeInt32 :: ObjectID -> Int32 -> IO ()
rawInvokeDerivedTypeInt32 d s = putStrLn "DerivedType.Foo(Int32)"



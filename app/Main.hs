{-# LANGUAGE TypeApplications, DataKinds #-}

module Main where

import Clr
import Bindings()

import Data.Int(Int32, Int64)

main :: IO ()
main = do
  base <- new @(ClrType "BaseType" '[]) ()                                 -- Constructors
  derived <- new @(ClrType "DerivedType" '[]) ()
  putStrLn ""
  invokeS @(ClrType "WriteLine" '[]) @(ClrType "System.Console" '[]) "Hi!"               -- Static method invocation
  invokeS @(ClrType "WriteLine" '[]) @(ClrType "System.Console" '[]) ("Hello", "Again")  -- Overloaded
  putStrLn ""
  invokeI @(ClrType "Foo" '[]) base "hi"                                   -- Instance method invocation
  invokeI @(ClrType "Foo" '[]) base (2::Int32)
  invokeI @(ClrType "Foo" '[]) base (2::Int64)
  putStrLn ""
  invokeI @(ClrType "Foo" '[]) derived "hi"
  invokeI @(ClrType "Foo" '[]) derived (2::Int32)
  invokeI @(ClrType "Foo" '[]) derived (2::Int64)
  putStrLn ""
  invokeI @(ClrType "Bar" '[]) base "hi"
  invokeI @(ClrType "Bar" '[]) base (2::Int32)
  invokeI @(ClrType "Bar" '[]) base (2::Int64)
  putStrLn ""
  invokeI @(ClrType "Bar" '[]) derived "hi"                               -- DerivedType doesn't implement Bar so should call it on base type
  invokeI @(ClrType "Bar" '[]) derived (2::Int32)
  invokeI @(ClrType "Bar" '[]) derived (2::Int64)
  putStrLn ""
  myGenType <- new @(ClrType "MyGenType" '[ClrType "System.String" '[]]) () -- Generic type
  invokeI @(ClrType "Add" '[]) myGenType "hello"
--  invokeI @(ClrType "Add" '[]) myGenType (2::Int32)                       -- This would be a compilation error
  putStrLn ""


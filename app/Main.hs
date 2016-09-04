{-# LANGUAGE TypeApplications, DataKinds #-}

module Main where

import Clr
import Bindings()

import Data.Int(Int32, Int64)

main :: IO ()
main = do
  base <- new @(T "BaseType") ()                                 -- Constructors
  derived <- new @(T "DerivedType") ()
  putStrLn ""
  invokeS @(T "WriteLine") @(T "System.Console") "Hi!"               -- Static method invocation
  invokeS @(T "WriteLine") @(T "System.Console") ("Hello", "Again")  -- Overloaded
  putStrLn ""
  invokeI @(T "Foo") base "hi"                                   -- Instance method invocation
  invokeI @(T "Foo") base (2::Int32)
  invokeI @(T "Foo") base (2::Int64)
  putStrLn ""
  invokeI @(T "Foo") derived "hi"
  invokeI @(T "Foo") derived (2::Int32)
  invokeI @(T "Foo") derived (2::Int64)
  putStrLn ""
  invokeI @(T "Bar") base "hi"
  invokeI @(T "Bar") base (2::Int32)
  invokeI @(T "Bar") base (2::Int64)
  putStrLn ""
  invokeI @(T "Bar") derived "hi"                               -- DerivedType doesn't implement Bar so should call it on base type
  invokeI @(T "Bar") derived (2::Int32)
  invokeI @(T "Bar") derived (2::Int64)
  putStrLn ""
  myGenType <- new @(GT "MyGenType" '[T "System.String"]) () -- Generic type
  invokeI @(T "Add") myGenType "hello"
--  invokeI @(T "Add") myGenType (2::Int32)                       -- This would be a compilation error
  putStrLn ""


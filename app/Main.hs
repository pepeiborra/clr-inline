{-# LANGUAGE TypeApplications, DataKinds #-}

module Main where

import Clr
import Bindings()

import Data.Int(Int32, Int64)

main :: IO ()
main = do
  invokeS @"WriteLine" @"System.Console" "implicit hi"
  invokeS @"WriteLine" @"System.Console" ("hello", "again implicit")
  putStrLn ""
  invokeI @"Foo" baseInstance "hi"
  invokeI @"Foo" baseInstance (2::Int32)
  invokeI @"Foo" baseInstance (2::Int64)
  putStrLn ""
  invokeI @"Foo" derivedInstance "hi"
  invokeI @"Foo" derivedInstance (2::Int32)
  invokeI @"Foo" derivedInstance (2::Int64)
  putStrLn ""
  invokeI @"Bar" baseInstance "hi"
  invokeI @"Bar" baseInstance (2::Int32)
  invokeI @"Bar" baseInstance (2::Int64)
  putStrLn ""
  invokeI @"Bar" derivedInstance "hi"
  invokeI @"Bar" derivedInstance (2::Int32)
  invokeI @"Bar" derivedInstance (2::Int64)

baseInstance :: Object "BaseType"
baseInstance = Object 1

derivedInstance :: Object "DerivedType"
derivedInstance = Object 1

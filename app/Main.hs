{-# LANGUAGE TypeApplications, DataKinds #-}

module Main where

import Clr
import Bindings()

import Data.Int(Int32, Int64)

main :: IO ()
main = do
  base <- new @"BaseType" ()                                 -- Constructors
  derived <- new @"DerivedType" ()
  putStrLn ""
  invokeS @"WriteLine" @"System.Console" "Hi!"               -- Static method invocation
  invokeS @"WriteLine" @"System.Console" ("Hello", "Again")  -- Overloaded
  putStrLn ""
  invokeI @"Foo" base "hi"                                   -- Instance method invocation
  invokeI @"Foo" base (2::Int32)
  invokeI @"Foo" base (2::Int64)
  putStrLn ""
  invokeI @"Foo" derived "hi"
  invokeI @"Foo" derived (2::Int32)
  invokeI @"Foo" derived (2::Int64)
  putStrLn ""
  invokeI @"Bar" base "hi"
  invokeI @"Bar" base (2::Int32)
  invokeI @"Bar" base (2::Int64)
  putStrLn ""
  invokeI @"Bar" derived "hi"                                -- DerivedType doesn't implement Bar so should call it on base type
  invokeI @"Bar" derived (2::Int32)
  invokeI @"Bar" derived (2::Int64)


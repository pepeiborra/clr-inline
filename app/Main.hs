{-# LANGUAGE TypeApplications, DataKinds #-}

module Main where

import Clr
import Bindings()

import Data.Int(Int32, Int64)

main :: IO ()
main = do
  invokeS @"WriteLine" @"System.Console" "implicit hi"
  invokeS @"WriteLine" @"System.Console" ("hello", "again implicit")
  invokeI @"MakeSound" someInstance "hi"
  invokeI @"MakeSound" someInstance (2::Int32)
  invokeI @"MakeSound" someInstance (2::Int64)

someInstance :: Object "MyInstance"
someInstance = Object 1


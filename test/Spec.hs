{-# LANGUAGE TypeApplications, TypeInType, TypeFamilies #-}

import Clr
import Bindings()

import Data.Int(Int32, Int64)

-- not really a proper test suite but something that should compile with expected output that is manually looked at

type instance SuperTypes (T "1" '[]) = '[]
type instance SuperTypes (T "2" '[]) = '[ T "1" '[] ]
type instance SuperTypes (T "3" '[]) = '[]
type instance SuperTypes (T "4" '[]) = '[ T "2" '[], T "3" '[]]
type instance SuperTypes (T "5" '[]) = '[ T "1" '[], T "3" '[]]
type instance SuperTypes (T "6" '[]) = '[]
type instance SuperTypes (T "7" '[]) = '[]
type instance SuperTypes (T "8" '[]) = '[ T "6" '[], T "7" '[], T "5" '[]]
type instance SuperTypes (T "9" '[]) = '[ T "8" '[] ]


main :: IO ()
main = do
  putStrLn ""
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
  putStrLn ""
  myGenType <- new @'("MyGenType", "System.String") ()       -- Generic type
  invokeI @"Add" myGenType "hello"
--  invokeI @"Add" myGenType (2::Int32)                      -- This would be a compilation error
  putStrLn ""


{-# LANGUAGE TypeApplications, TypeInType, TypeFamilies #-}

import Test.Hspec

import Clr
import Instances()

import Data.Int(Int32, Int64)

-- TODO: I used the next lines once for manual testing in GHCI. The order of finding the super type matters when looking up the base type
-- Turn these next lines into something testable again.
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
  base <- new @"BaseType" ()                                 :: IO (Object (T "BaseType" '[]))                       -- Constructors
  derived <- new @"DerivedType" ()                           :: IO (Object (T "DerivedType" '[]))

  invokeS @"WriteLine" @"System.Console" "Hi!"              `shouldReturn` "Console.WriteLine(String)"               -- Static method invocation
  invokeS @"WriteLine" @"System.Console" ("Hello", "Again") `shouldReturn` "Console.WriteLine(String,String)"        -- Overloaded

  invokeI @"Foo" base "hi"                                  `shouldReturn` "BaseType.Foo(String)"                    -- Instance method invocation
  invokeI @"Foo" base (2::Int32)                            `shouldReturn` "BaseType.Foo(Int32)"
  invokeI @"Foo" base (2::Int64)                            `shouldReturn` "BaseType.Foo(Int64)"

  invokeI @"Foo" derived "hi"                               `shouldReturn` "DerivedType.Foo(String)"
  invokeI @"Foo" derived (2::Int32)                         `shouldReturn` "DerivedType.Foo(Int32)"
  invokeI @"Foo" derived (2::Int64)                         `shouldReturn` "DerivedType.Foo(Int64)"

  invokeI @"Bar" base "hi"                                  `shouldReturn` "BaseType.Bar(String)"
  invokeI @"Bar" base (2::Int32)                            `shouldReturn` "BaseType.Bar(Int32)"
  invokeI @"Bar" base (2::Int64)                            `shouldReturn` "BaseType.Bar(Int64)"

  invokeI @"Bar" derived "hi"                               `shouldReturn` "BaseType.Bar(String)"                    -- DerivedType doesn't implement Bar so should call it on base type
  invokeI @"Bar" derived (2::Int32)                         `shouldReturn` "BaseType.Bar(Int32)"
  invokeI @"Bar" derived (2::Int64)                         `shouldReturn` "BaseType.Bar(Int64)"

  myGenType <- new @'("MyGenType", "System.String") ()       :: IO (Object (T "MyGenType" '[T "System.String" '[]])) -- Generic type
  invokeI @"Add" myGenType "hello"                          `shouldReturn` "MyGenType.Add(String)"
--  invokeI @"Add" myGenType (2::Int32)                      -- This would be a compilation error


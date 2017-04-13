{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Clr.Host
import Clr.CSharp.Inline
import Clr.FSharp.Inline
import Data.Int
import Data.Text as Text (pack)
import Test.Hspec

[csharp|
using System;|]

[fsharp|
open System |]

main :: IO ()
main = do
  startClr
  [csharp|
         Console.WriteLine();
         Console.WriteLine("Hello CLR inline !!!");
         Console.WriteLine(2);
         Console.WriteLine("The year is {0}", 2017);
         Console.WriteLine("Well {0} {1}", "This", "Is Cool");
         return;
         |]
  [fsharp| printfn "And this is %d in F#" (System.DateTime.Today.Year) |]
  let h_i   = 2 :: Int
  let h_i32 = 2 :: Int32
  let h_i64 = 2 :: Int64
  let h_d = 2.2 :: Double
  let h_b = False
  let h_s = "Hello from Haskell"
  let h_t = Text.pack h_s

  i <- [fsharp| int{DateTime(2017,01,01).Year} |]
  h_i'   <- [fsharp| int { $h_i:int + $h_i}|]
  h_i32' <- [fsharp| int32 { $h_i32:int32 + 0}|]
  h_i64' <- [fsharp| int64 { $h_i64:int64 + 0L}|]
  d <- [fsharp| double{ 2.0 * $h_d:double} |]
  s <- [fsharp| string{"Hello"}|]
  t <- [fsharp| text{"Hello text"}|]
  w <- [fsharp| word{2}|]
  o <- [fsharp| object{ DateTime(2017,04,10)} |]

  [fsharp| printfn "%s" $h_s:string |]
  [fsharp| printfn "%s" $h_t:text|]

  -- requires better types on the F# side
  --  (which in turn requires better types on the H side)
  -- d <- [fsharp| ($o:Object).Day |]

  i `shouldBe` 2017
  h_i' `shouldBe` h_i * 2
  h_i32' `shouldBe` h_i32
  h_i64' `shouldBe` h_i64
  d `shouldBe` h_d * 2
  s `shouldBe` "Hello"
  t `shouldBe` Text.pack "Hello text"
  w `shouldBe` 2

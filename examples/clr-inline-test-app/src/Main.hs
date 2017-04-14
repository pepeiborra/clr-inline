{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Clr.Inline
import Data.Int
import Data.Text as Text (pack)
import Test.Hspec

[csharp|
using System;|]

[fsharp|
open System
open System.Collections.Generic
|]

-- Note that "System.DateTime" and "DateTime" are not the same type in our simple model!!
type SystemDateTime = Object "System.DateTime"
type DateTime = Object "DateTime"

main :: IO ()
main = do
  startClr

  --
  -- C# examples
  --
  [csharp| Console.WriteLine("Hello CLR inline !!!"); |]
  i <- [csharp| int { return 2; }|]
  i_array <- [csharp| int[] {
                    int[] a = new int[4]{0,0,0,0};
                    for(int i=0; i < 4; i++) {
                      a[i] = i;
                    }
                    return a;
                    }|]
  i `shouldBe` 2
  print =<< [csharp| int{return ($i_array:int[])[3];}|]

  today <- [csharp| System.DateTime{ return System.DateTime.Today;}|]
  [csharp| Console.WriteLine( ($today:System.DateTime).ToString());|]
  --
  -- F# examples
  --
  _ :: () <- [fsharp| printfn "The result of this expression is discarded" |]

  -- locals
  let h_i   = 2 :: Int
  let h_i32 = 2 :: Int32
  let h_i64 = 2 :: Int64
  let h_d = 2.2 :: Double
  let h_b = False
  let h_s = "Hello from Haskell"
  let h_t = Text.pack h_s

  -- Examples of antiquotation
  i      <- [fsharp| int   { DateTime(2017,01,01).Year} |]
  h_i'   <- [fsharp| int   { $h_i:int + $h_i}|]
  h_i32' <- [fsharp| int32 { $h_i32:int32 + 0}|]
  h_i64' <- [fsharp| int64 { $h_i64:int64 + 0L}|]
  d      <- [fsharp| double{ 2.0 * $h_d:double} |]
  s      <- [fsharp| string{ "Hello"}|]
  t      <- [fsharp| text  { "Hello text"}|]
  w      <- [fsharp| word  { 2}|]
  [fsharp| printfn "%s" $h_s:string |]
  [fsharp| printfn "%s" $h_t:text|]
  --
  -- reference type examples
  o :: DateTime <- [fsharp| DateTime{ DateTime(2017,04,10)} |]
  day    <- [fsharp| int{($o:DateTime).Day} |]
  array  <- [fsharp| DateTime[]{
                      [ DateTime.Today; DateTime.Now ] |> Array.ofList }
                    |]
  print =<< [fsharp| int{ ($array:DateTime[]).[0].Hour}|]
  print =<< [fsharp| int{ ($array:DateTime[]).[1].Hour}|]

  dict <- [fsharp| Map<int,string> {
                    [ 1,"Foo" ; 2, "bar" ] |> Map.ofSeq
                 }|]
  print =<< [fsharp| string{ ($dict:Map<int,string>).[1] }|]

  i `shouldBe` 2017
  h_i' `shouldBe` h_i * 2
  h_i32' `shouldBe` h_i32
  h_i64' `shouldBe` h_i64
  d `shouldBe` h_d * 2
  s `shouldBe` "Hello"
  t `shouldBe` Text.pack "Hello text"
  w `shouldBe` 2
  day `shouldBe` 10

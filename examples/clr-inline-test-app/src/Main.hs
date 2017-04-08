{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Clr.Host
import Clr.CSharp.Inline
import Clr.FSharp.Inline
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

  i <- [fsharp| int{DateTime(2017,01,01).Year} |]
  b <- [fsharp| bool { 2>4}|]
  f <- [fsharp| float{ 0.5} |]
  d <- [fsharp| double{ -0.6} |]
  s <- [fsharp| string{"Hello"}|]
  w <- [fsharp| word{2}|]
  i `shouldBe` 2017
  b `shouldBe` False
  f `shouldBe` 0.5
  d `shouldBe` -0.6
  s `shouldBe` "Hello"
  w `shouldBe` 2

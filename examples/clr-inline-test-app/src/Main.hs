{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Clr.Host
import Clr.CSharp.Inline
import Clr.FSharp.Inline

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

  i <- [fsharp|int{DateTime.Today.Year} |]
  b <- [fsharp| bool { 2>4}|]
  f <- [fsharp| float{ 0.5} |]
  d <- [fsharp| double{ 0.5} |]
  -- string is a work in progress
  print i
  print b
  print f
  print d

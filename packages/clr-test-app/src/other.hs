{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Other where

import Clr.FSharp.Inline
import Foreign.Ptr

[fsharp|
open System |]

main2 = do
  i <- [fsharp|int{DateTime.Today.Year} |]
  b <- [fsharp| bool { 2>4}|]
  f <- [fsharp| float{ 0.5} |]
  d <- [fsharp| double{ 0.5} |]
  -- string is a work in progress
  print i
  print b
  print f
  print d

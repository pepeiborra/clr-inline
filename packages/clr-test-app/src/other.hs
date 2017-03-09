{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Other where

import Clr.FSharp.Inline
import Foreign.Ptr

[fsharp|
open System |]

main2 =
  [fsharp| printfn "And this is an inline F# call from another module (%d)" DateTime.Today.Year |]

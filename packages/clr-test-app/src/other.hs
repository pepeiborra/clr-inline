{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module Other where

import Clr.FSharp.Inline
import Foreign.Ptr

main2 =
  [fsharp| printfn "And this is an inline F# call from another module" |]

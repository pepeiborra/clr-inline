{-# LANGUAGE StaticPointers     #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DataKinds #-}
module Bench where

import Clr.Inline
import Criterion
import Criterion.Main
import Data.Text

i :: Int
i = 7

s :: String
s = "Hello world"

t :: Text
t = pack "Hello World"

[fsharp|
module Global =
   let today = System.DateTime.Today
   let hello = "Hello World" |]

main :: IO ()
main = do
  startClr
  o <- [fsharp| System.DateTime{System.DateTime.Today}|]
  defaultMain
    [
      bench "invoke" $ whnfIO [fsharp| () |]
    , bench "unmarshal int" $ whnfIO [fsharp| int { 7 } |]
    , bench "unmarshal string" $ whnfIO [fsharp| string { Global.hello }|]
    , bench "unmarshal text" $ whnfIO [fsharp| text{ Global.hello }|]
    , bench "unmarshal object" $ whnfIO [fsharp| System.DateTime{Global.today}|]
    , bench "marshal int" $ whnfIO [fsharp| ignore <| $i:int|]
    , bench "marshal string" $ whnfIO [fsharp| ignore <| $s:string|]
    , bench "marshal text" $ whnfIO [fsharp| ignore <| $t:text|]
    , bench "marshal object" $ whnfIO [fsharp| ignore <| $o:System.DateTime|]
    ]

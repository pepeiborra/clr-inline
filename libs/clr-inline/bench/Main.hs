{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeInType #-}

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

main :: IO ()
main = do
  startClr
  o <- [fsharp| System.DateTime{System.DateTime.Today}|]
  defaultMain
    [
      bench "invoke" $ whnfIO [fsharp| () |]
    , bench "unmarshal int" $ whnfIO [fsharp| int { 7 } |]
    , bench "unmarshal string" $ whnfIO [fsharp| string { "Hello world" }|]
    , bench "unmarshal text" $ whnfIO [fsharp| text{ "Hello world" }|]
    , bench "marshal int" $ whnfIO [fsharp| $i:int|]
    , bench "marshal string" $ whnfIO [fsharp| $s:string|]
    , bench "marshal text" $ whnfIO [fsharp| $t:text|]
    , bench "marshal object" $ whnfIO [fsharp| $o:System.DateTime|]
    , bench "unmarshal object" $ whnfIO [fsharp| System.DateTime{System.DateTime.Today}|]
    ]

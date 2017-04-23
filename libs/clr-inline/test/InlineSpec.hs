{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module InlineSpec where

import Control.Concurrent
import Control.Monad
import Clr.Inline
import Data.Int
import Data.Text as Text (pack)
import Data.Word
import Test.Hspec
import System.Mem
import System.Mem.Weak

[csharp| using System;
         using System.Collections.Generic;
       |]

[fsharp|
open System
open System.Collections.Generic
|]

type SystemDateTime = Clr "System.DateTime"
type DateTime = Clr "DateTime"

main = hspec spec

h_i   = 2 :: Int
h_i16 = 2 :: Int16
h_i32 = 2 :: Int32
h_i64 = 2 :: Int64
h_w16 = 1 :: Word16
h_w32 = 1 :: Word32
h_w64 = 1 :: Word64
h_d = 2.2 :: Double
h_b = False
h_s = "Hello from Haskell"
h_t = Text.pack h_s

spec :: Spec
spec = beforeAll_ startClr $ do

  it "F# inlines pick up imports" $
    [fsharp| ignore <| Dictionary<int,int>() |]

  it "C# inlines pick up imports" $
    [csharp| var foo = new Dictionary<int,int>(); return ;|]

  it "F# inlines return all primitive types" $ do
    [fsharp| int{2}|] `shouldReturn` 2
    [fsharp| bool{false}|] `shouldReturn` False
    [fsharp| double{2.2}|] `shouldReturn` 2.2
    [fsharp| int16{2s}|] `shouldReturn` 2
    [fsharp| int32{2}|] `shouldReturn` 2
    [fsharp| int64{2L}|] `shouldReturn` 2
    [fsharp| long{2L}|] `shouldReturn` 2
    [fsharp| uint16{2us}|] `shouldReturn` 2
    [fsharp| word16{2us}|] `shouldReturn` 2
    [fsharp| uint32{2u}|] `shouldReturn` 2
    [fsharp| word32{2u}|] `shouldReturn` 2
    [fsharp| uint64{2UL}|] `shouldReturn` 2
    [fsharp| word64{2UL}|] `shouldReturn` 2
    [fsharp| text{"hello"}|] `shouldReturn` pack "hello"
    [fsharp| string{"hello"}|] `shouldReturn` "hello"

  it "F# inlines return reference types" $ do
    date <- [fsharp| DateTime{ DateTime.Today}|]
    return ()

  it "F# supports antiquoting int" $
    [fsharp|bool{$h_i:int = 2}|] `shouldReturn` True
  it "F# supports antiquoting int16" $
    [fsharp|bool{$h_i16:int16 = 2s}|] `shouldReturn` True
  it "F# supports antiquoting int32" $
    [fsharp|bool{$h_i32:int32 = 2}|] `shouldReturn` True
  it "F# supports antiquoting int64" $
    [fsharp|bool{$h_i64:int64 = 2L}|] `shouldReturn` True
  it "F# supports antiquoting long" $
    [fsharp|bool{$h_i64:long  = 2L}|] `shouldReturn` True
  it "F# supports antiquoting uint16" $
    [fsharp|bool{$h_w16:uint16 = 1us}|] `shouldReturn` True
  it "F# supports antiquoting uint32" $
    [fsharp|bool{$h_w32:uint32 = 1u}|] `shouldReturn` True
  it "F# supports antiquoting uint64" $
    [fsharp|bool{$h_w64:uint64 = 1UL}|] `shouldReturn` True
  it "F# supports antiquoting bool" $
    [fsharp|bool{$h_b:bool = false}|] `shouldReturn` True
  it "F# supports antiquoting double" $
    [fsharp|bool{$h_d:double = 2.2}|] `shouldReturn` True
  it "F# supports antiquoting string" $
    [fsharp|bool{$h_s:string = "Hello from Haskell"}|] `shouldReturn` True
  it "F# supports antiquoting text" $
    [fsharp|bool{$h_t:text = "Hello from Haskell"}|] `shouldReturn` True

  it "F# handles two antiquotations" $
    [fsharp|bool{$h_i32:int32 + $h_i:int = 4}|] `shouldReturn` True

  it "F# tuples are handled correctly" $ do
    tuple <- [fsharp| int*bool{8,true}|]
    [fsharp| bool{snd $tuple:int*bool}|] `shouldReturn` True
    [fsharp| int {fst $tuple:int*bool}|] `shouldReturn` 8

  it "Reference types are released when Haskell GCs them" $ do
    !tuple <-
      [fsharp|WeakReference*Object{
           let d = obj()
           let w = WeakReference(d)
           (w,d)}|]
    !w <- [fsharp|WeakReference{fst $tuple:WeakReference*Object}|]
    gcUntil [fsharp|bool{System.GC.Collect(); not ($w:WeakReference).IsAlive}|]

  it "But not any earlier" $ do
    tuple <-
      [fsharp|WeakReference*Object{
           let d = Object()
           let w = WeakReference(d)
           if not (Object.ReferenceEquals(w.Target,d)) then
               failwithf "This is not a good test(%O,%O)" w.Target d
           (w,d)}|]
    w <- [fsharp|WeakReference{fst $tuple:WeakReference*Object}|]
    d <- [fsharp|Object{snd $tuple:WeakReference*Object}|]
    performGC
    threadDelay 50000
    [fsharp|bool{
           GC.Collect()
           let res = ($w:WeakReference).IsAlive
           GC.KeepAlive($d:Object)
           res}
           |] `shouldReturn` True

  it "F# generics are handled" $ do
        dict <- [fsharp| Map<int,string> {
                    [ 1,"Foo" ; 2, "bar" ] |> Map.ofSeq
                 }|]
        [fsharp| string{ ($dict:Map<int,string>).[1] }|]
          `shouldReturn` "Foo"

  it "C# arrays are handled" $ do
      i_array <- [csharp| int[] {
                        int[] a = new int[4]{0,0,0,0};
                        for(int i=0; i < 4; i++) {
                          a[i] = i;
                        }
                        return a;
                        }|]
      [csharp|int{return ($i_array:int[])[2];}|] `shouldReturn` 2

gcUntil cond = do
  let loop 10 = error "gc: tried too many times"
      loop i  = do
        performGC
        threadDelay (i * 10000)
        success <- cond
        unless success $ loop (i+1)
  loop 0

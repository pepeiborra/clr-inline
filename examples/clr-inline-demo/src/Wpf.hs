{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StaticPointers     #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Clr.Inline
import WpfDeps

[wpf|
open System.IO
open System.Threading
open System.Windows
open System.Windows.Controls
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Shapes
open System.Xml
|]

main = withClr $
  [wpf|
      let loadXamlWindow (xaml:string) =
          use stream = new StringReader(xaml)
          let reader = XmlReader.Create(stream)
          XamlReader.Load(reader) :?> Window
      let mutable result = 0
      use wait = ManualResetEvent(false)
      let t = Thread(fun() ->
        let app = new Application()
        let w = loadXamlWindow($xaml:string)
        app.MainWindow <- w
        let e = w.FindName("Circle") :?> Ellipse
        e.MouseLeftButtonUp.Add(fun _ ->
          e.Fill <-
              if e.Fill = (Brushes.Yellow :> Brush) then Brushes.Red
              else Brushes.Yellow )
        result <- app.Run(w)
        wait.Set() |> ignore
        )
      t.SetApartmentState(ApartmentState.STA)
      t.IsBackground = false
      ignore <| t.Start()
      wait.WaitOne() |> ignore
      result
         |]

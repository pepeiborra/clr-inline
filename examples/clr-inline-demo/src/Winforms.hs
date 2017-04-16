
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where

import Clr.Inline

[fsharp|
  open System.Windows.Forms
       |]

main = do
  startClr
  let text = "Hello from Haskell"
  [fsharp|
        let form = new Form(Text=$text:string)
        let button = new Button(Text="Click Me!", Dock=DockStyle.Fill)
        button.Click.Add(fun _ -> MessageBox.Show($text, "Hey!") |> ignore)
        form.Controls.Add(button)
        Application.Run(form)
         |]

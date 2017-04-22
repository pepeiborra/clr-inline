# clr-inline: Call into the CLR by writing C# / F# inline in Haskell modules
[![Unix build status](https://gitlab.com/tim-m89/clr-haskell/badges/master/build.svg)](https://gitlab.com/tim-m89/clr-haskell/commits/master)[![Windows Build Status](https://img.shields.io/appveyor/ci/tim-m89/clr-haskell.svg?label=Windows%20build)](https://ci.appveyor.com/project/pepeiborra/clr-haskell)

**NOTE: you will need GHC >= 8.2 to use this package in Windows.

Following on recent packages like `inline-java`, `inline-c` and `inline-r`,
this package provides a simple and direct FFI bridge to call CLR functions
from Haskell, without the need to write any boilerplate FFI declarations.
`clr-inline` is implemented on top of the [clr-host][clr-host] and
[clr-bindings][clr-bindings] packages.

[clr-host]: clr-host/
[clr-bindings]: clr-bindings/

## Example

Graphical hello world using F# Winforms:

```haskell
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
```
## Configuration
The quasiquoters in this package look for the F#/C# compiler binaries in the
application path. External dependencies and search paths can be provided to
the quasiquoter as configuration. Configuration creates a new quasiquoter;
since GHC does not allow calling a quasiquoter from the same module where it is
defined, the recommended practice is to configure the quasiquoters in a 
dedicated Config module. Example configuration for WPF dependencies:

```haskell
module WpfDeps where

import Clr.Inline
import Clr.Inline.Config

wpf =
  fsharp' $
    defaultConfig
    { configDependencies =
        [ "System.Xaml"
        , "WindowsBase"
        , "PresentationCore"
        , "PresentationFramework"
        ]
    }
```


## LICENSE

Copyright (c) 2017 Jose Iborra

clr-inline is free software and may be redistributed under the terms
specified in the [LICENSE](LICENSE) file.

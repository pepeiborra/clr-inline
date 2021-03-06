clr-inline
==============
[![Hackage](https://img.shields.io/hackage/v/clr-inline.svg)](https://hackage.haskell.org/package/clr-inline)
[![Stackage Nightly](http://stackage.org/package/clr-inline/badge/nightly)](http://stackage.org/nightly/package/clr-inline)
[![Unix build status](https://gitlab.com/tim-m89/clr-haskell/badges/master/build.svg)](https://gitlab.com/tim-m89/clr-haskell/commits/master)
[![Windows Build Status](https://img.shields.io/appveyor/ci/TimMatthews/clr-haskell.svg?label=Windows%20build)](https://ci.appveyor.com/project/tim-m89/clr-haskell)

NOTE: you will need GHC >= 8.2 to use this package in Windows.

What is clr-inline
=======================
**clr-inline** provides a quasiquoter to inline F# and C# code in Haskell modules. 
It was inspired by [`inline-java`], [`inline-c`] and [`inline-r`], and it is implemented
on top of [clr-host][clr-host] and [clr-marshal][clr-marshal] packages.

[clr-host]: https://gitlab.com/tim-m89/clr-haskell/tree/master/libs/clr-host
[clr-marshal]: https://gitlab.com/tim-m89/clr-haskell/tree/master/libs/clr-marshal
[inline-java]: http://hackage.haskell.org/package/inline-java
[inline-r]: http://hackage.haskell.org/package/inline-r
[inline-c]: http://hackage.haskell.org/package/inline-c

Example
==========

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
Features
==========
* Inline F# / C# in Haskell.
* Automatic unmarshalling of CLR primitive types into Haskell.
* Reference types support including arrays and generics.
* Refer to (`Quotable`) Haskell values inside F# / C# quotations.
* Experimental support for Haskell lambdas in F#.
  * See the InlineSpec.hs test module for examples of usage.
  * Only mscorlib types are admitted, type resolution will crash at runtime otherwise.

Getting Started
===================
Install the `clr-inline` package from Hackage using your preferred package manager:

    $ cabal install clr-inline
    $ stack install clr-inline

By default, `.Net` is used in Windows and `mono` in other systems. 
This is driven by Cabal flags in the `clr-host` package.

Requirements
================
`clr-inline` requires GHC >=8.0 for `mono`, and either GHC >=8.2 or a [linker preprocessor] for `.Net`.

[linker preprocessor]: https://gitlab.com/tim-m89/clr-haskell/tree/master/utils/clr-win-linker

Cabal requires that the CLR compiler is in the application path at `cabal configure` time. 
The module `Clr.Inline.Cabal` provides an optional Cabal user hook that can be added to a cabal
 Setup script to check for this automatically

Bug [#14090] in GHC 8.2.1 requires that the **StaticPointers** language extension is turned on in every module that
contains an inline F# or C# block. Moreover, the modules must have an unrestricted export list.

[#14090]: http://ghc.haskell.org/trac/ghc/ticket/14090

The quasiquoters look for the F#/C# compiler binaries in the
application path. External dependencies and additional search paths can be provided to
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


LICENSE
==========

Copyright (c) 2017 Jose Iborra

clr-inline is free software and may be redistributed under the terms
specified in the [LICENSE](LICENSE) file.

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StaticPointers     #-}
{-# LANGUAGE TemplateHaskell #-}
module WpfDeps where

import Clr.Inline
import Clr.Inline.Config
import Data.String.Here

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

xaml = [hereFile|src/Window.xaml|]

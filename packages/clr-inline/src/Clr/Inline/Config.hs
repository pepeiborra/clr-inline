{-# LANGUAGE TemplateHaskell #-}
module Clr.Inline.Config where

import Clr.Host.Config
import System.FilePath
import Language.Haskell.TH

data ClrInlineConfig = ClrInlineConfig
  { configFSharpPath :: FilePath
  , configCSharpPath :: FilePath
  , configDependencies :: [String]
  , configExtraIncludeDirs :: [FilePath]
  , configDebugSymbols :: Bool
  , configCustomCompilerFlags :: [String]
  , configForceReturnType :: Maybe TypeQ
  }

defaultMonoConfig = ClrInlineConfig "fsharpc" "mcs" [] [] False [] (Just [t|()|])
defaultDotNetConfig  = ClrInlineConfig "fsc" "csc" [] [] False [] (Just [t|()|])
defaultInlineConfig = case defaultHostConfig of
                  ClrHostConfig ClrHostMono -> defaultMonoConfig
                  ClrHostConfig ClrHostDotNet -> defaultDotNetConfig

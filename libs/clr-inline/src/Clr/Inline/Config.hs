module Clr.Inline.Config where

import Clr.Host.Config

data ClrInlineConfig = ClrInlineConfig
  { configFSharpPath :: FilePath
  , configCSharpPath :: FilePath
  , configDependencies :: [String]
  , configExtraIncludeDirs :: [FilePath]
  , configDebugSymbols :: Bool
  , configCustomCompilerFlags :: [String]
  }

defaultMonoConfig, defaultDotNetConfig, defaultConfig :: ClrInlineConfig
defaultMonoConfig = ClrInlineConfig "fsharpc" "mcs" [] [] False []
defaultDotNetConfig  = ClrInlineConfig "fsc" "csc" [] [] False []
defaultConfig = case defaultHostConfig of
                  ClrHostConfig ClrHostMono -> defaultMonoConfig
                  ClrHostConfig ClrHostDotNet -> defaultDotNetConfig

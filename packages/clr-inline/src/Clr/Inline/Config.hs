module Clr.Inline.Config where

import Clr.Host.Config
import System.FilePath

data ClrInlineConfig = ClrInlineConfig
  { configFSharpPath :: FilePath
  , configCSharpPath :: FilePath
  , configDependencies :: [String]
  , configExtraIncludeDirs :: [FilePath]
  , configDebugSymbols :: Bool
  , configCustomCompilerFlags :: [String]
  }
  deriving Show

defaultMonoConfig = ClrInlineConfig "fsharpc" "mcs" [] [] False []
defaultDotNetConfig  = ClrInlineConfig "fsc" "csc" [] [] False []
defaultInlineConfig = case defaultHostConfig of
                  ClrHostConfig ClrHostMono -> defaultMonoConfig
                  ClrHostConfig ClrHostDotNet -> defaultDotNetConfig

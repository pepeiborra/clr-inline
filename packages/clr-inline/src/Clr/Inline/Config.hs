module Clr.Inline.Config where

import Clr.Host.Config
import System.FilePath

data ClrInlineConfig = Config
  { configFSharpPath :: FilePath
  , configCSharpPath :: FilePath
  , configDependencies :: [FilePath]
  }
  deriving Show

defaultMonoConfig = Config "fsharpc" "mcs" []
defaultDotNetConfig  = Config "fsc" "csc" []
defaultInlineConfig = case defaultHostConfig of
                  ClrHostConfig ClrHostMono -> defaultMonoConfig
                  ClrHostConfig ClrHostDotNet -> defaultDotNetConfig

module AssemblyDeps.Config where

import Clr.Inline hiding (fsharp)
import Clr.Inline.Config

fsharp = fsharp' defaultConfig{ configDependencies = ["lib/fsharplib.dll"] }

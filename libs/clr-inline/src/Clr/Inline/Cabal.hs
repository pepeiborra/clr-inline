module Clr.Inline.Cabal (ensureFSharp, ensureCSharp) where

import Clr.Inline.Config
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Verbosity as Verbosity

-- | Add this to your Cabal Setup.hs driver in order to require the
--   the F# compiler is in the path.
--
-- @
--  import Clr.Inline.Cabal
--  import Distribution.Simple
--
--  main = defaultMainWithHooks $ ensureFSharp simpleUserHooks
-- @
--
ensureFSharp :: UserHooks -> UserHooks
ensureFSharp userHooks =
  userHooks {confHook = check fsharpCompiler (confHook userHooks)}

-- | Add this to your Cabal Setup.hs driver in order to require the
--   the C# compiler is in the path.
ensureCSharp :: UserHooks -> UserHooks
ensureCSharp userHooks =
  userHooks {confHook = check csharpCompiler (confHook userHooks)}

fsharpCompiler, csharpCompiler :: Program
csharpCompiler = simpleProgram (configCSharpPath defaultConfig)
fsharpCompiler = simpleProgram (configFSharpPath defaultConfig)

check :: Program -> (gh -> cf -> IO LocalBuildInfo) -> gh -> cf -> IO LocalBuildInfo
check pgm base gh cf = do
  lbi <- base gh cf
  _ <- requireProgram Verbosity.normal pgm (withPrograms lbi)
  return lbi

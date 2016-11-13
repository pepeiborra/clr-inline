import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.Configure(configure)
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import Distribution.Simple.Program
import Distribution.Verbosity as Verbosity

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Maybe
import System.Directory
import System.FilePath

main = defaultMainWithHooks simpleUserHooks { confHook  = configureClrHost
                                            , buildHook = buildClrHost     }

buildClrHost :: PackageDescription -> LocalBuildInfo -> UserHooks -> BuildFlags -> IO ()
buildClrHost pd lbi hooks bflags = do
  let verbosity = fromFlagOrDefault Verbosity.normal (buildVerbosity bflags)
  let progDb = withPrograms lbi
  currentDir <- getCurrentDirectory
  let src = currentDir </> "src" </> "Driver.cs"
  let out = currentDir </> "src" </> "Driver.dll"
  runDbProgram verbosity csharpCompiler progDb ["-filealign:512", "-optimize+", "-out:" ++ out, "-target:library", src]
  buildHook simpleUserHooks pd lbi hooks bflags

configureClrHost :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
configureClrHost (gpd,hbi) cf = do
  lbi <- confHook simpleUserHooks (gpd, hbi) cf
  let verbosity = fromFlagOrDefault Verbosity.normal (configVerbosity (configFlags lbi))
  (cfgdProg, progDb) <- requireProgram verbosity csharpCompiler (withPrograms lbi)
  let lbi'   = lbi { withPrograms = progDb }  :: LocalBuildInfo
  return lbi'

cscFindLocation :: Verbosity -> ProgramSearchPath -> IO (Maybe (FilePath, [FilePath]))
cscFindLocation verb path = runMaybeT $ findExec "mcs"
                                    <|> findExec "csc"
                                    <|> findExec "C:\\Windows\\Microsoft.NET\\Framework64\\v4.0.30319\\csc"
                          where findExec name = MaybeT $ findProgramOnSearchPath verb path name

csharpCompiler :: Program
csharpCompiler = (simpleProgram "csc") { programFindLocation = cscFindLocation }


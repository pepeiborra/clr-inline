
import Clr.Inline.Cabal
import Distribution.Simple

main = defaultMainWithHooks $ ensureFSharp simpleUserHooks

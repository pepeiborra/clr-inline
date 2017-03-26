module Main where

import Control.Monad
import Data.List
import System.Environment
import System.Process
import System.IO
import System.Directory
import Pipes
import Pipes.Safe
import qualified Pipes.Safe.Prelude as SP
import Pipes.Prelude

runLinker :: [String] -> IO ()
runLinker args = callProcess "gcc" args

modifyLinkerArgs :: Pipe String String (SafeT IO) ()
modifyLinkerArgs = do
  x <- await
  if x == "\"-Xlinker\""
    then do
      y <- await
      if y == "\"--stack=0x800000,0x800000\""
        then yield "\"-fstack-check\""
      else yield x >> yield y
    else yield x
  modifyLinkerArgs

prependWith :: a -> Pipe a a (SafeT IO) ()
prependWith x = do
  yield x
  forever $ await >>= yield

main :: IO ()
main = do
  args <- getArgs
  case args of
    (  []  ) -> error "No args"
    (x : xs) -> do
      let linkFileOrig = tail x
      let linkFileNew  = linkFileOrig ++ "-temp"
      runSafeT $ runEffect $ SP.readFile linkFileOrig >-> modifyLinkerArgs >-> tee (SP.writeFile linkFileNew) >-> prependWith "New linker args:" >-> stdoutLn
      renameFile linkFileNew linkFileOrig
      runLinker args

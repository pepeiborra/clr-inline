{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StaticPointers #-}
module AssemblyDeps where

import AssemblyDeps.Config
import Clr.Inline hiding (fsharp)
import System.Directory
import System.IO

[fsharp| open fsharplib |]

main = withClr $ do
  cd <- getCurrentDirectory
  putStrLn cd
  [fsharp| void { Class1().Run() }|]

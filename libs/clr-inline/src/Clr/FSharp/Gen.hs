{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Clr.FSharp.Gen (name, compile) where

import           Clr.Inline.Config
import           Clr.Inline.Quoter
import           Clr.Inline.Utils
import           Clr.Inline.Utils.Embed
import           Clr.Inline.Types
import           Control.Monad
import           Control.Monad.Trans.Writer
import qualified Data.ByteString                 as BS
import           Data.ByteString.Char8           (ByteString)
import           Data.List
import qualified Data.Map as Map
import           Data.String.Here.Uninterpolated
import           System.Directory
import           System.FilePath                 ((<.>), (</>))
import           System.IO.Temp
import           System.Process
import           Text.Printf

data FSharp
name :: String
name = "fsharp"

genCode :: ClrInlinedGroup FSharp -> String
genCode ClrInlinedGroup {..} =
  unlines $
  execWriter $ do
    yield $ printf "namespace %s" modNamespace
    forM_ units $ \case
      ClrInlinedDec body -> yield body
      ClrInlinedUnit {} -> return ()
    yield $ printf "module %s = " modName
    forM_ units $ \case
      ClrInlinedDec {} -> return ()
      ClrInlinedUnit {..} -> do
        yield $ printf   "    let %s (%s) ="
            (getMethodName name unitId)
            (intercalate ", " [printf "%s:%s" a t | (a, ClrType t) <- Map.toList args])
        yield            "        try "
        forM_ (lines body) $ \l ->
          yield $ printf "                       %s" l
        yield            "        with e -> printfn \"Inline F# threw an exception:\\n %O\" e ; reraise()"

compile :: ClrInlineConfig -> ClrInlinedGroup FSharp -> IO ClrBytecode
compile ClrInlineConfig {..} m@ClrInlinedGroup {..} = do
  temp <- getTemporaryDirectory
  dir <- createTempDirectory temp "inline-fsharp"
  let src = dir </> modName <.> ".fs"
      tgt = dir </> modName <.> ".dll"
  writeFile src (genCode m)
  callCommand $
    unwords $
    execWriter $ do
      yield configFSharpPath
      yield "--nologo"
      yield "--target:library"
      yield $ "--out:" ++ tgt
      when configDebugSymbols $ yield "--debug:embedded"
      forM_ configExtraIncludeDirs $ \dir -> yield $ "--lib:" ++ dir
      forM_ configDependencies $ \name -> yield $ "--reference:" ++ name
      yieldAll configCustomCompilerFlags
      yield src
  bcode <- BS.readFile tgt
  return $ ClrBytecode bcode

body :: ByteString
body = [hereFile|src/introspect.fs|]

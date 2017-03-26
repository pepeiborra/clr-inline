{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Clr.FSharp.Gen (compile) where

import           Clr.Inline.Config
import           Clr.Inline.Utils
import           Clr.Inline.Types
import           Control.Monad
import           Control.Monad.Trans.Writer
import qualified Data.ByteString     as BS
import           Data.List
import           Data.Typeable
import           Language.Haskell.TH
import           System.Directory
import           System.FilePath     ((<.>), (</>))
import           System.IO.Temp
import           System.Process
import           Text.Printf

data FSharp

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
        yield $
          printf
            "    let %s (%s) = "
            name
            (intercalate ", " $ zipWith (printf "%s:$s") args argTypes)
        forM_ (lines body) $ \l -> yield $ printf "        %s" l

compile :: ClrInlineConfig -> ClrInlinedGroup FSharp -> IO ClrBytecode
compile config@ClrInlineConfig {..} m@ClrInlinedGroup {..} = do
  temp <- getTemporaryDirectory
  dir <- createTempDirectory temp "inline-fsharp"
  let src = dir </> modName <.> ".fs"
      tgt = dir </> modName <.> ".dll"
  writeFile src (genCode m)
  putStrLn $ "Generated " ++ tgt
  callCommand $
    unwords $
    execWriter $ do
      yield configFSharpPath
      yield "--target:library"
      yield $ "--out:" ++ tgt
      when configDebugSymbols $ yield "--debug:embedded"
      forM_ configExtraIncludeDirs $ \dir -> yield $ "--lib:" ++ dir
      forM_ configDependencies $ \name -> yield $ "--reference:" ++ name
      yieldAll configCustomCompilerFlags
      yield src
  bcode <- BS.readFile tgt
  return $ ClrBytecode bcode

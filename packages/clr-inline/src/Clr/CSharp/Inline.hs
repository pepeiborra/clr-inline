{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Clr.CSharp.Inline (csharp, csharp', FunPtr, getMethodStub) where

import           Clr.Bindings
import           Clr.Inline.Config
import           Clr.Inline.Types
import           Clr.Inline.Utils
import           Control.Monad
import           Control.Monad.Trans.Writer
import qualified Data.ByteString            as BS
import           Data.List
import           Foreign
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           System.Directory
import           System.FilePath     ((<.>), (</>))
import           System.IO.Temp
import           System.Process
import           Text.Printf

csharp = csharp' defaultInlineConfig

csharp' cfg = QuasiQuoter
    { quoteExp  = csharpExp cfg
    , quotePat  = error "Clr.CSharp.Inline: quotePat"
    , quoteType = error "Clr.CSharp.Inline: quoteType"
    , quoteDec  = csharpDec cfg
    }

csharpExp :: ClrInlineConfig -> String -> Q Exp
csharpExp cfg = clrQuoteExp "csharp" $ compile cfg
csharpDec cfg = clrQuoteDec "csharp" $ compile cfg

data CSharp

genCode :: ClrInlinedGroup CSharp -> String
genCode ClrInlinedGroup {..} =
  unlines $
  execWriter $ do
    yield $ printf "namespace %s {" modNamespace
    forM_ units $ \case
      ClrInlinedDec {..} ->
        yield body
      ClrInlinedUnit{} ->
        return ()
    yield $ printf "public class %s {" modName
    forM_ units $ \case
      ClrInlinedDec{} ->
        return ()
      ClrInlinedUnit {..} -> do
        yield $
          printf
            "    public static void %s (%s) { "
            name
            (intercalate ", " $ zipWith (printf "%s $s") argTypes args)
        forM_ (lines body) $ \l -> do yield $ printf "        %s" l
        yield "}"
    yield "}}"

compile :: ClrInlineConfig -> ClrInlinedGroup CSharp -> IO ClrBytecode
compile ClrInlineConfig{..} m@ClrInlinedGroup {..} = do
    temp <- getTemporaryDirectory
    dir <- createTempDirectory temp "inline-csharp"
    let src = dir </> modName <.> ".cs"
        tgt = dir </> modName <.> ".dll"
    writeFile src (genCode m)
    putStrLn $ "Generated " ++ tgt
    callCommand $
      unwords $
      execWriter $ do
        yield configCSharpPath
        yield "-target:library"
        yield $ "-out:" ++ tgt
        when configDebugSymbols $ yield "-debug"
        forM_ configExtraIncludeDirs $ \dir -> yield $ "-lib:" ++ dir
        forM_ configDependencies $ \name -> yield $ "-reference:" ++ name
        yieldAll configCustomCompilerFlags
        yield src
    bcode <- BS.readFile tgt
    return $ ClrBytecode bcode

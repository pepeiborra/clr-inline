{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Clr.CSharp.Inline (csharp, csharp', FunPtr, getMethodStubRaw, BStr(..)) where

import           Clr.Bindings
import           Clr.Bindings.Host
import           Clr.Inline.Config
import           Clr.Inline.Quoter
import           Clr.Inline.Utils
import           Clr.Inline.Utils.Embed
import           Clr.Inline.Types
import           Control.Monad
import           Control.Monad.Trans.Writer
import qualified Data.ByteString            as BS
import           Data.List
import qualified Data.Map as Map
import           Foreign
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           System.Directory
import           System.FilePath            ((<.>), (</>))
import           System.IO.Temp
import           System.Process
import           Text.Printf

csharp :: QuasiQuoter
csharp = csharp' defaultInlineConfig
name :: [Char]
name = "csharp"

csharp' :: ClrInlineConfig -> QuasiQuoter
csharp' cfg = QuasiQuoter
    { quoteExp  = csharpExp cfg
    , quotePat  = error "Clr.CSharp.Inline: quotePat"
    , quoteType = error "Clr.CSharp.Inline: quoteType"
    , quoteDec  = csharpDec cfg
    }

csharpExp :: ClrInlineConfig -> String -> Q Exp
csharpExp cfg =
  clrQuoteExp
    name
    (configForceReturnType cfg)
    (compile cfg)
csharpDec :: ClrInlineConfig -> String -> Q [Dec]
csharpDec cfg = clrQuoteDec name $ compile cfg

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
            (getMethodName name unitId)
            (intercalate ", " [printf "%s:%s" a t | (a, ClrType t) <- Map.toList args])
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

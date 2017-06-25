{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeInType          #-}
module Clr.CSharp.Inline (csharp, csharp') where

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
import           Data.Proxy
import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           System.Directory
import           System.FilePath            ((<.>), (</>))
import           System.IO.Temp
import           System.Process
import           Text.Printf

-- | Quasiquoter for C# declarations and expressions.
--   A quasiquote is a block of C# statements wrapped in curly braces
--   preceded by the C# return type.
--   Examples:
--
-- @
-- example :: IO (Clr "int[]")
-- example = do
--  [csharp| Console.WriteLine("Hello CLR inline !!!"); |]
--  i <- [csharp| int { return 2; }|]
--  [csharp| int[] {  int[] a = new int[4]{0,0,0,0};
--                    for(int i=0; i < 4; i++) {
--                      a[i] = i;
--                    }
--                    return a;
--                 }|]
-- @
--
--   See the documentation for 'fsharp' for details on the quotation
--   and antiquotation syntaxes.
--  This quasiquoter is implicitly configured with the 'defaultConfig'.
csharp :: QuasiQuoter
csharp = csharp' defaultConfig

name :: Proxy "csharp"
name = Proxy

-- | Explicit configuration version of 'csharp'.
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
    (compile cfg)
csharpDec :: ClrInlineConfig -> String -> Q [Dec]
csharpDec cfg = clrQuoteDec name $ compile cfg


genCode :: ClrInlinedGroup "csharp" -> String
genCode ClrInlinedGroup {units, mod} =
  unlines $
  execWriter $ do
    yield $ printf "namespace %s {" (getNamespace mod)
    forM_ units $ \case
      ClrInlinedDec _ body ->
        yield body
      ClrInlinedExp{} ->
        return ()
    yield $ printf "public class %s {" (getClassName mod)
    forM_ units $ \case
      ClrInlinedDec{} ->
        return ()
      ClrInlinedExp exp@ClrInlinedExpDetails {..} -> do
        yield $
          printf
            "    public static %s %s (%s) { "
            returnType
            (getMethodName exp)
            (intercalate ", " [ printf "%s %s" t a
                              | (a, argDetails) <- Map.toList args
                              , let t = case argDetails of
                                          Value (ClrType t) -> t
                                          Delegate{} -> error "delegates not yet supported in the C# quoter."
                              ])
        yield $ printf "#line %d \"%s\"" (fst $ loc_start loc) (loc_filename loc)
        forM_ (lines body) $ \l -> yield $ printf "        %s" l
        yield "}"
    yield "}}"

compile :: ClrInlineConfig -> ClrInlinedGroup "csharp" -> IO ClrBytecode
compile ClrInlineConfig{..} m@ClrInlinedGroup {..} = do
    temp <- getTemporaryDirectory
    dir <- createTempDirectory temp "inline-csharp"
    let ass = getAssemblyName name mod
    let src = dir </> ass <.> ".cs"
        tgt = dir </> ass <.> ".dll"
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

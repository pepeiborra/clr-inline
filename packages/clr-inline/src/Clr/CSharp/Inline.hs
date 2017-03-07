{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
module Clr.CSharp.Inline (csharp, FunPtr, getMethodStub) where

import           Clr.Bindings
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

csharp = QuasiQuoter
    { quoteExp  = csharpExp
    , quotePat  = error "Clr.CSharp.Inline: quotePat"
    , quoteType = error "Clr.CSharp.Inline: quoteType"
    , quoteDec  = error "Clr.CSharp.Inline: quoteDec"
    }

csharpExp :: String -> Q Exp
csharpExp = clrQuoteExp "csharp" compile

-- The name of the C# compiler in Mono.
-- Platform specific
csharpCompiler = "mcs"

data CSharp

yield x = tell [x]

genCode :: ClrInlinedGroup CSharp -> String
genCode ClrInlinedGroup {..} =
  unlines $
  execWriter $ do
    yield $ printf "namespace %s {" modNamespace
    yield $ "using System;" -- TODO imports
    yield $ printf "public class %s {" modName
    forM_ units $ \ClrInlinedUnit {..} -> do
      yield $
        printf
          "    public static void %s (%s) { "
          name
          (intercalate ", " $ zipWith (printf "%s $s") argTypes args)
      forM_ (lines body) $ \l -> do yield $ printf "        %s" l
      yield "}"
    yield "}}"

compile :: ClrInlinedGroup CSharp -> IO ClrBytecode
compile m@ClrInlinedGroup {..} = do
    temp <- getTemporaryDirectory
    dir <- createTempDirectory temp "inline-csharp"
    let src = dir </> modName <.> ".cs"
        tgt = dir </> modName <.> ".dll"
    writeFile src (genCode m)
    putStrLn $ "Generated " ++ tgt
    callProcess csharpCompiler ["-target:library", "-out:"++tgt, src]
    bcode <- BS.readFile tgt
    return $ ClrBytecode bcode

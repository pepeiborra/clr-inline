{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Clr.FSharp.Inline (fsharp) where

import Control.Monad.IO.Class
import Clr.Bindings
import Clr.Inline.State
import Clr.Inline.Utils
import Clr.FSharp.Gen
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Foreign
import Text.Printf

namespace = "Clr.FSharp.Inline"

fsharp = QuasiQuoter
    { quoteExp  = fsharpExp
    , quotePat  = error "Clr.FSharp.Inline: quotePat"
    , quoteType = error "Clr.FSharp.Inline: quoteType"
    , quoteDec  = error "Clr.FSharp.Inline: quoteDec"
    }

-- | Runs after the whole module has been loaded and is responsible for generating:
--     - A clr assembly with all the inline code, embedding it into the module.
genFSharp = do
  FinalizerState {wrappers} <- getFinalizerState @ FSharpUnit
  modName <- mangleModule <$> thisModule
  let mod = FSharpModule modName namespace wrappers
  result <- runIO $ compile mod

  -- Embed the bytecodes
  embedBytecode =<< runIO(compile mod)
  return ()

-- | Quasiquoter for expressions. Responsible for:
--      - Installing a finalizer to generate the bytecodes
--      - Generating the foreign import wrapper.
--      - Splicing in the computation that loads the bytecodes, gets a function pointer through the keyhole, and calls it.
fsharpExp :: String -> Q Exp
fsharpExp body = do
  methodName <- (\d -> "quote_" ++ show d) <$> getFinalizerCount @ FSharpUnit
  stubName <- newName "stub"
  -- TODO support for antiquotations
  let args = [] :: [Q Exp]
  let argTypes = [] :: [String]
  modName <- mangleModule <$> thisModule
  let assemblyName = modName
  pushWrapperGen genFSharp $ return (FSharpUnit methodName body argTypes)
  let fullClassName :: String =
        printf
          "%s.%s, %s, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null"
          namespace
          modName
          assemblyName
  -- Generate the top level foreign import calls needed to convert the function pointers into Haskell functions
  -- TODO Add support for argument and result type inference
  resTy <- [t|IO ()|]
  let argTys = []
  let funTy = foldr AppT resTy argTys
  lookupTypeName "Foreign.Ptr.FunPtr" >>= \case
    Nothing -> error "Please import Foreing.Ptr when using F# quotations"
    Just funPtrTy ->
        qAddTopDecls
          [ ForeignD
              (ImportF
                CCall
                Safe
                "dynamic"
                stubName
                (ArrowT `AppT` AppT (ConT funPtrTy) funTy `AppT` funTy))
          ]
  -- splice in the bytecode load and call to the stub
  [|unembedBytecode >>
    getMethodStub $(lift fullClassName) $(lift methodName) $(lift argTypes) >>=
    return . $(varE stubName) >>= \f -> $(foldr appE [|f|] args)|]

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

namespace = "Clr.FSharp.Inline"

fsharp = QuasiQuoter
    { quoteExp  = fsharpExp
    , quotePat  = error "Clr.FSharp.Inline: quotePat"
    , quoteType = error "Clr.FSharp.Inline: quoteType"
    , quoteDec  = error "Clr.FSharp.Inline: quoteDec"
    }

-- | Runs after the whole module has been loaded and is responsible for generating:
--     - A clr assembly with all the inline code, embedding it into the module.
--     - The foreign import wrappers.
genFSharp = do
  FinalizerState {wrappers} <- getFinalizerState @ FSharpUnit
  modName <- mangleModule <$> thisModule
  let mod = FSharpModule modName namespace wrappers
  result <- runIO $ compile mod
  -- TODO Embed the bytecodes
  -- TODO Add support for argument and result type inference
  resTy <- [t|IO ()|]
  let argTys = []

  -- Generate the top level foreign import calls needed to convert the function pointers into Haskell functions
  qAddTopDecls
    [ ForeignD (ImportF CCall Safe "dynamic" wrapperName (foldr AppT resTy argTys))
    | FSharpUnit{..} <- wrappers ]
  return ()

-- | Quasiquoter for expressions. Responsible for:
--      - Installing a finalizer to generate the bytecodes and foreign import wrappers
--      - Splicing in the computation that loads the bytecodes, gets a function pointer through the keyhole, and calls it.
fsharpExp :: String -> Q Exp
fsharpExp body = do
  name <- newName "dynamicWrapper"
  stubName <- newName "stub"
  -- TODO support for antiquotations
  let args = [] :: [Q Exp]
  let argTypes = [] :: [String]
  pushWrapperGen genFSharp $ return (FSharpUnit (show name) body name argTypes)
  className <- mangleModule <$> thisModule
  [| loadBytecodes >>
     getMethodStub $(lift className) $(lift $ show name) $(lift argTypes) >>=
     return . $(varE stubName) >>=
     \f -> f $(listE args)
   |]

-- | This function is responsible for finding the clr assembly embedded in this module and loading it into the clr runtime
loadBytecodes :: IO ()
loadBytecodes = return () -- TODO

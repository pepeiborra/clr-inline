{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Clr.Inline.Types where

import Clr.Inline.State
import Clr.Inline.Utils
import Data.ByteString (ByteString)
import Data.Typeable
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Printf

data ClrInlinedUnit language
  = ClrInlinedUnit { name :: String
                  ,  body :: String
                  ,  args :: [String]
                  ,  argTypes :: [String]}
  | ClrInlinedDec { body :: String}

data ClrInlinedGroup language = ClrInlinedGroup
  { modName :: String
  , modNamespace :: String
  , units   :: [ClrInlinedUnit language]
  }

namespace = "Clr.Inline"

-- | Runs after the whole module has been loaded and is responsible for generating:
--     - A clr assembly with all the inline code, embedding it into the module.
clrGenerator
  :: Typeable language
  => String -> String -> (ClrInlinedGroup language -> IO ClrBytecode) -> Q ()
clrGenerator name modName compile = do
  FinalizerState {wrappers} <- getFinalizerState
  let mod = ClrInlinedGroup modName namespace wrappers
  result <- runIO $ compile mod

  -- Embed the bytecodes
  embedBytecode name =<< runIO(compile mod)
  return ()

-- | Quasiquoter for expressions. Responsible for:
--      - Installing a finalizer to generate the bytecodes
--      - Generating the foreign import wrapper.
--      - Splicing in the computation that loads the bytecodes, gets a function pointer through the keyhole, and calls it.
clrQuoteExp
  :: forall language.
     Typeable language
  => String -> (ClrInlinedGroup language -> IO ClrBytecode) -> String -> Q Exp
clrQuoteExp name clrCompile body = do
  count <- getFinalizerCount @(ClrInlinedGroup language)
  methodName <- newName $ printf "%s_quote_%d" name count
  stubName <- newName $ printf "%s_stub_%d" name count
  -- TODO support for antiquotations
  let args = [] :: [Exp]
  let argTypes = [] :: [String]
  modName <- mangleModule name <$> thisModule
  let assemblyName = modName
  pushWrapperGen (clrGenerator name modName clrCompile) $
    return (ClrInlinedUnit (show methodName) body (map show args) argTypes :: ClrInlinedUnit language)
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
    getMethodStub $(lift fullClassName) $(lift $ show methodName) $(lift argTypes) >>=
    return . $(varE stubName) >>= \f -> $(foldr appE [|f|] (map return args))|]

-- | Quasi quoter for declaration in the clr language.
--   Does not splice anything onto the Haskell source.
clrQuoteDec :: forall language . Typeable language => String -> (ClrInlinedGroup language -> IO ClrBytecode) -> String -> Q [Dec]
clrQuoteDec name clrCompile body = do
  modName <- mangleModule name <$> thisModule
  pushWrapperGen (clrGenerator name modName clrCompile) $
    return (ClrInlinedDec body :: ClrInlinedUnit language)
  return mempty

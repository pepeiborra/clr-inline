{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Clr.Inline.Types where

import Clr.Inline.State
import Clr.Inline.Utils
import Control.Monad
import Data.ByteString (ByteString)
import Data.Typeable
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Printf

data ClrInlinedUnit language
  = ClrInlinedUnit { unitId :: Int
                  ,  body :: String
                  ,  args :: [String]
                  ,  argTypes    :: [Type]
                  ,  argClrTypes :: [String]
                  ,  stubName    :: Name
                  ,  returnType  :: Type}
  | ClrInlinedDec { body :: String}

data ClrInlinedGroup language = ClrInlinedGroup
  { modName :: String
  , modNamespace :: String
  , units   :: [ClrInlinedUnit language]
  }

namespace = "Clr.Inline"
getStubName, getMethodName :: String -> Int -> String
getStubName name count = printf "%s_stub_%d" name count
getMethodName name count = printf "%s_quote_%d" name count
getFullClassName modName =
  printf
    "%s.%s, %s, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null"
    namespace
    modName
    modName
    :: String

generateFFIStub ClrInlinedUnit {..} = do
  let funTy = return $ foldr AppT returnType argTypes
      -- This is what we'd like to write:
      -- [d| foreign import ccall "dynamic" $stubName :: $([t|FunPtr $funTy -> $funTy|]) |]
      -- Unfort. splicing names into foreign import decl is not supported, so we have to write:
  ForeignD <$> ImportF CCall Safe "dynamic" stubName <$> [t|FunPtr $funTy -> $funTy|]

-- | Runs after the whole module has been loaded and is responsible for generating:
--     - A clr assembly with all the inline code, embedding it into the module.
clrGenerator
  :: Typeable language
  => String -> String -> (ClrInlinedGroup language -> IO ClrBytecode) -> Q ()
clrGenerator name modName compile = do
  FinalizerState {wrappers} <- getFinalizerState
  let mod = ClrInlinedGroup modName namespace wrappers
  resultTypes <- runIO $ compile mod

  -- Embed the bytecodes
  embedBytecode name =<< runIO(compile mod)

  -- generate the stub wrappers
  -- TODO Add support for argument and result type inference
--  mapM (generateFFIStub) wrappers >>= qAddTopDecls

unliftClrType :: Type -> String
unliftClrType = undefined

-- | Quasiquoter for expressions. Responsible for:
--      - Installing a finalizer to generate the bytecodes
--      - Generating the foreign import wrapper.
--      - Splicing in the computation that loads the bytecodes, gets a function pointer through the keyhole, and calls it.
clrQuoteExp
  :: forall language.
     Typeable language
  => String -> TypeQ -> (ClrInlinedGroup language -> IO ClrBytecode) -> String -> Q Exp
-- TODO support for return type inference
clrQuoteExp name returnType clrCompile body = do
  count <- getFinalizerCount @(ClrInlinedGroup language)
  -- TODO support for antiquotations
  let args = [] :: [Exp]
  let argTypes = [] :: [Type]
  modName <- mangleModule name <$> thisModule
  stubName <- newName $ getStubName name count
  let methodName = getMethodName name count
  let fullClassName = getFullClassName modName
  let argClrTypes = map unliftClrType argTypes
  resTy <- [t| IO $(returnType) |]
  let inlinedUnit :: ClrInlinedUnit language =
        ClrInlinedUnit
          count
          (normaliseLineEndings body)
          (map show args)
          argTypes
          argClrTypes
          stubName
          resTy
  pushWrapperGen (clrGenerator name modName clrCompile) $ return inlinedUnit

  generateFFIStub inlinedUnit >>= qAddTopDecls . (:[])

  --
  -- splice in the bytecode load and call to the stub
  [|unembedBytecode >>
    getMethodStub
      $(lift fullClassName)
      $(lift methodName)
      argClrTypes >>=
    return . $(varE stubName) >>= \f -> $(foldr appE [|f|] (map return args))|]

-- | Quasi quoter for declaration in the clr language.
--   Does not splice anything onto the Haskell source.
clrQuoteDec :: forall language . Typeable language => String -> (ClrInlinedGroup language -> IO ClrBytecode) -> String -> Q [Dec]
clrQuoteDec name clrCompile body = do
  modName <- mangleModule name <$> thisModule
  pushWrapperGen (clrGenerator name modName clrCompile) $
    return (ClrInlinedDec (normaliseLineEndings body) :: ClrInlinedUnit language)
  return mempty

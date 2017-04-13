{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
module Clr.Inline.Quoter where

import Clr.Host.BStr
import Clr.Marshal
import Clr.Inline.State
import Clr.Inline.Types
import Clr.Inline.Utils
import Clr.Inline.Utils.Args
import Clr.Inline.Utils.Embed
import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Printf

data ClrInlinedUnit language argType
  = ClrInlinedUnit { unitId :: Int
                  ,  body :: String
                  ,  args :: Map String argType
                  ,  stubName :: Name
                  ,  fullClassName :: String
                  ,  methodName :: String
                  ,  returnType  :: Type }
  | ClrInlinedDec { body :: String}

makeLensesFor [("args","_args")] ''ClrInlinedUnit

data ClrInlinedGroup language = ClrInlinedGroup
  { modName :: String
  , modNamespace :: String
  , units :: [ClrInlinedUnit language ClrType]
  }

namespace :: [Char]
namespace = "Clr.Inline"
getStubName, getMethodName :: String -> Int -> String
getStubName name count = printf "%s_stub_%d" name count
getMethodName name count = printf "%s_quote_%d" name count
getFullClassName :: PrintfArg t => t -> String
getFullClassName modName =
  printf
    "%s.%s, %s, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null"
    namespace
    modName
    modName
    :: String

toClrArg :: [Char] -> [Char]
toClrArg x = "arg_" ++ x

toClrTypeOrFail :: Type -> ClrType
toClrTypeOrFail x =
  fromMaybe
    (error $ "Failed to convert to clr type: " ++ show (ppr x))
    (toClrType x)

getValueName :: String -> Q Name
getValueName a = fromMaybe (error $ "Identifier not in scope: " ++ a) <$> lookupValueName a

generateFFIStub :: ClrInlinedUnit t Type -> Q [Dec]
generateFFIStub ClrInlinedUnit {..} = do
  let funTy = return $ foldr (\t u -> ArrowT `AppT` t `AppT` u) returnType (Map.elems args)
  -- This is what we'd like to write:
  -- [d| foreign import ccall "dynamic" $stubName :: $([t|FunPtr $funTy -> $funTy|]) |]
  -- Unfort. splicing names into foreign import decl is not supported, so we have to write:
  -- TODO Convert every type to its Marshalled counterpart
  ffiStub <- ForeignD . ImportF CCall Safe "dynamic" stubName <$> [t|FunPtr $funTy -> $funTy|]
  return [ffiStub]

generateClrCall :: ClrInlinedUnit t a -> ExpQ
generateClrCall ClrInlinedUnit{..} = do
  let argExps =
        [ [| marshal $(varE =<< getValueName a)|]
        | a <- Map.keys args
        ]
  [| do unembedBytecode
        stub <- marshal $(lift fullClassName) $ \c -> marshal $(lift methodName) $ \m -> getMethodStubRaw >>= \f -> return $ f c m (BStr nullPtr)
        let stub_f = $(varE stubName) stub
        result <- $(foldr appE [|stub_f|] (argExps))
        unmarshalAuto result
    |]

-- | Runs after the whole module has been loaded and is responsible for generating:
--     - A clr assembly with all the inline code, embedding it into the module.
clrGenerator
  :: forall language . Typeable language
  => String -> String -> (ClrInlinedGroup language -> IO ClrBytecode) -> Q ()
clrGenerator name modName compile = do
  FinalizerState {wrappers} <- getFinalizerState @(ClrInlinedUnit language Type)
  let typedWrappers =
        over (traversed . _args) (Map.mapKeysMonotonic toClrArg . Map.map toClrTypeOrFail) wrappers
  let mod = ClrInlinedGroup modName namespace typedWrappers
  _ <- runIO $ compile mod
  -- Embed the bytecodes
  embedBytecode name =<< runIO (compile mod)


-- | Quasiquoter for expressions. Responsible for:
--      - Installing a finalizer to generate the bytecodes
--      - Generating the foreign import wrapper.
--      - Splicing in the computation that loads the bytecodes, gets a function pointer through the keyhole, and calls it.
clrQuoteExp
  :: forall language.
     Typeable language
  => String -> Maybe TypeQ -> (ClrInlinedGroup language -> IO ClrBytecode) -> String -> Q Exp
clrQuoteExp name returnType clrCompile body = do
  count <- getFinalizerCount @(ClrInlinedUnit language Type)
  modName <- mangleModule name <$> thisModule
  stubName <- newName $ getStubName name count
  let methodName = getMethodName name count
  let fullClassName = getFullClassName modName
  let (parsedBody, resTy :: TypeQ) =
        case (returnType, parseBody body) of
          (Nothing, Left _err) -> (body, [t|()|])
          (Just ty, Left _err) -> (body, ty)
          (Nothing, Right bodyAndRet) -> bodyAndRet
          (Just ty, Right (body', _)) -> (body', ty)
  let (antis, parsedBody') = extractArgs toClrArg parsedBody
  resTy <- [t|IO $(resTy)|]
  argsTyped <- traverse (\x -> maybe (error $ "Cannot parse type: " ++ x) snd (toTHType x)) antis
  let inlinedUnit :: ClrInlinedUnit language Type =
        ClrInlinedUnit
          count
          (normaliseLineEndings parsedBody')
          argsTyped
          stubName
          fullClassName
          methodName
          resTy
  pushWrapperGen (clrGenerator name modName clrCompile) $ return inlinedUnit
  --
  -- splice in a proxy datatype for the late bound class, used to delay the type checking of the stub call
  addTopDecls =<< generateFFIStub inlinedUnit
  --
  -- splice in the bytecode load and call to the stub
  generateClrCall inlinedUnit

-- | Quasi quoter for declaration in the clr language.
--   Does not splice anything onto the Haskell source.
clrQuoteDec :: forall language . Typeable language => String -> (ClrInlinedGroup language -> IO ClrBytecode) -> String -> Q [Dec]
clrQuoteDec name clrCompile body = do
  modName <- mangleModule name <$> thisModule
  pushWrapperGen (clrGenerator name modName clrCompile) $
    return (ClrInlinedDec (normaliseLineEndings body) :: ClrInlinedUnit language Type)
  return mempty

unmarshalAuto :: Unmarshal a (UnmarshalAs a) => a -> IO(UnmarshalAs a)
unmarshalAuto = unmarshal

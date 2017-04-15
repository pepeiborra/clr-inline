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

import Clr.Bindings.Host (GetMethodStubDelegate, makeGetMethodStubDelegate, unsafeGetPointerToMethod)
import Clr.Host.BStr
import Clr.Marshal
import Clr.Inline.State
import Clr.Inline.Types
import Clr.Inline.Utils
import Clr.Inline.Utils.Args
import Clr.Inline.Utils.Embed
import Control.Lens
import Data.Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Typeable
import Foreign.Ptr
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Unsafe
import Text.Printf

data ClrInlinedExpDetails argType = ClrInlinedExpDetails
  { unitId :: Int
  , body :: String
  , args :: Map String argType
  , stubName :: Name
  , fullClassName :: String
  , methodName :: String
  , returnType :: String
  }

data ClrInlinedUnit language argType
  = ClrInlinedExp (ClrInlinedExpDetails argType)
  | ClrInlinedDec String

makePrisms ''ClrInlinedUnit
makeLensesFor [("args","_args")] ''ClrInlinedExpDetails

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

generateFFIStub :: ClrInlinedExpDetails Type -> Q [Dec]
generateFFIStub ClrInlinedExpDetails{..} = do
  resTy <- [t| IO $(fst $ toTHType returnType)|]
  let funTy = return $ foldr (\t u -> ArrowT `AppT` t `AppT` u) resTy (Map.elems args)
  -- This is what we'd like to write:
  -- [d| foreign import ccall "dynamic" $stubName :: $([t|FunPtr $funTy -> $funTy|]) |]
  -- Unfort. splicing names into foreign import decl is not supported, so we have to write:
  -- TODO Convert every type to its Marshalled counterpart
  ffiStub <- ForeignD . ImportF CCall Safe "dynamic" stubName <$> [t|FunPtr $funTy -> $funTy|]
  return [ffiStub]

getMethodStubRaw :: (GetMethodStubDelegate a)
getMethodStubRaw = unsafeDupablePerformIO $ unsafeGetPointerToMethod "GetMethodStub" >>= return . makeGetMethodStubDelegate

invoke :: String -> String -> FunPtr a
invoke c m = unsafeDupablePerformIO $ marshal c $ \c -> marshal m $ \m -> return $ getMethodStubRaw c m (BStr nullPtr)

generateClrCall :: ClrInlinedExpDetails a -> ExpQ
generateClrCall ClrInlinedExpDetails{..} = do
  let argExps =
        [ [| marshal $(varE =<< getValueName a)|]
        | a <- Map.keys args
        ]
  let roll m f = [|$m . ($f .)|]
  [| do unembedBytecode
        let stub = invoke $(liftString fullClassName) $(liftString methodName)
        let stub_f = $(varE stubName) stub
        result <- $(foldr roll [|id|] (argExps)) stub_f
        unmarshalAuto (result)
    |]

-- | Runs after the whole module has been loaded and is responsible for generating:
--     - A clr assembly with all the inline code, embedding it into the module.
clrGenerator
  :: forall language . Typeable language
  => String -> String -> (ClrInlinedGroup language -> IO ClrBytecode) -> Q ()
clrGenerator name modName compile = do
  FinalizerState {wrappers} <- getFinalizerState @(ClrInlinedUnit language Type)
  let typedWrappers =
        over (traversed . _ClrInlinedExp . _args) (Map.mapKeysMonotonic toClrArg . Map.map toClrTypeOrFail) wrappers
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
  => String -> (ClrInlinedGroup language -> IO ClrBytecode) -> String -> Q Exp
clrQuoteExp name clrCompile body = do
  count <- getFinalizerCount @(ClrInlinedUnit language Type)
  modName <- mangleModule name <$> thisModule
  stubName <- newName $ getStubName name count
  let methodName = getMethodName name count
  let fullClassName = getFullClassName modName
  let (resultType, parsedBody) = parseBody body
  let (antis, parsedBody') = extractArgs toClrArg parsedBody
  argsTyped <- traverse (snd . toTHType) antis
  let inlinedUnit =
        ClrInlinedExpDetails
          count
          (normaliseLineEndings parsedBody')
          argsTyped
          stubName
          fullClassName
          methodName
          resultType
  pushWrapperGen (clrGenerator name modName clrCompile) $ return (ClrInlinedExp inlinedUnit :: ClrInlinedUnit language Type)
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
  pushWrapperGen (clrGenerator name modName clrCompile) $ do
    let nbody = normaliseLineEndings body
        ws =
          [ (white, line)
          | l <- lines nbody
          , let (white, line) = span isSpace l
          , not (null line)
          ]
        allEqual (x:y:xx) = x == y && allEqual (y : xx)
        allEqual _ = True
        body' =
          if allEqual (map fst ws)
            then unlines (map snd ws)
            else nbody
    return (ClrInlinedDec body' :: ClrInlinedUnit language Type)
  return mempty

unmarshalAuto :: Unmarshal a (UnmarshalAs a) => a -> IO(UnmarshalAs a)
unmarshalAuto = unmarshal

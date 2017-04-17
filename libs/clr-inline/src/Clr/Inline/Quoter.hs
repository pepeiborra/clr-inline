{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import GHC.TypeLits
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import System.IO.Unsafe
import Text.Printf

data ClrInlinedExpDetails (language :: Symbol) argType = ClrInlinedExpDetails
  { language :: Proxy language
  , unitId :: Int
  , stubName :: Name
  , body :: String
  , args :: Map String argType
  , returnType :: String
  }

data ClrInlinedUnit (language :: Symbol) argType
  = ClrInlinedExp (ClrInlinedExpDetails language argType)
  | ClrInlinedDec (Proxy language) String

makePrisms ''ClrInlinedUnit
makeLensesFor [("args","_args")] ''ClrInlinedExpDetails

data ClrInlinedGroup language = ClrInlinedGroup
  { mod :: Module
  , units :: [ClrInlinedUnit language ClrType]
  }

getNamespace :: Module -> String
getNamespace (Module (PkgName pkg) _) = printf "Clr.Inline.%s" pkg
getMethodName ClrInlinedExpDetails{..} = printf "%s_quote_%d" (symbolVal language) unitId
getMethodName :: KnownSymbol language => ClrInlinedExpDetails language a -> String
getClassName :: Module -> String
getClassName (Module _ (ModName n)) = n
getAssemblyName, getFullClassName :: KnownSymbol language => Proxy language -> Module -> String
getAssemblyName language (Module (PkgName p) (ModName m)) = printf "%s_%s_%s" p m (symbolVal language)
getFullClassName language mod =
  printf
    "%s.%s, %s, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null"
    (getNamespace mod)
    (getClassName mod)
    (getAssemblyName language mod)

toClrArg :: String -> String
toClrArg x = "arg_" ++ x


getValueName :: String -> Q Name
getValueName a = fromMaybe (error $ "Identifier not in scope: " ++ a) <$> lookupValueName a

generateFFIStub :: KnownSymbol language => ClrInlinedExpDetails language String -> Q [Dec]
generateFFIStub ClrInlinedExpDetails{..} = do
  resTy <- [t| IO $(lookupQuotableMarshalType returnType)|]
  argsTyped <- traverse lookupQuotableMarshalType args
  let funTy = return $ foldr (\t u -> ArrowT `AppT` t `AppT` u) resTy (Map.elems argsTyped)
  -- This is what we'd like to write:
  -- [d| foreign import ccall "dynamic" $stubName :: $([t|FunPtr $funTy -> $funTy|]) |]
  -- Unfort. splicing languages into foreign import decl is not supported, so we have to write:
  -- TODO Convert every type to its Marshalled counterpart
  ffiStub <- ForeignD . ImportF CCall Safe "dynamic" stubName <$> [t|FunPtr $funTy -> $funTy|]
  return [ffiStub]

getMethodStubRaw :: (GetMethodStubDelegate a)
getMethodStubRaw = unsafeDupablePerformIO $ makeGetMethodStubDelegate <$> unsafeGetPointerToMethod "GetMethodStub"

invoke :: String -> String -> FunPtr a
invoke c m = unsafeDupablePerformIO $ marshal c $ \c -> marshal m $ \m -> return $ getMethodStubRaw c m (BStr nullPtr)

generateClrCall :: KnownSymbol language => Module -> ClrInlinedExpDetails language a -> ExpQ
generateClrCall mod exp@ClrInlinedExpDetails{..} = do
  let argExps =
        [ [| marshal $(varE =<< getValueName a)|]
        | a <- Map.keys args
        ]
      roll m f = [|$m . ($f .)|]
  [| do unembedBytecode
        let stub = invoke $(liftString $ getFullClassName language mod) $(liftString $ getMethodName exp)
        let stub_f = $(varE stubName) stub
        result <- $(foldr roll [|id|] argExps) stub_f
        unmarshalAuto (Proxy :: $([t| Proxy $(litT $ strTyLit returnType) |])) result
    |]

-- | Runs after the whole module has been loaded and is responsible for generating:
--     - A clr assembly with all the inline code, embedding it into the module.
clrGenerator
  :: forall language . KnownSymbol language
  => Proxy language -> Module -> (ClrInlinedGroup language -> IO ClrBytecode) -> Q ()
clrGenerator language hsmod compile = do
  FinalizerState {wrappers} <- getFinalizerState @(ClrInlinedUnit language String)
  typedWrappers <-
        mapMOf (traversed . _ClrInlinedExp . _args) (fmap (Map.mapKeysMonotonic toClrArg) . traverse lookupQuotableClrType) wrappers
  let mod = ClrInlinedGroup hsmod typedWrappers
  _ <- runIO $ compile mod
  -- Embed the bytecodes
  embedBytecode (symbolVal language) =<< runIO (compile mod)


-- | Quasiquoter for expressions. Responsible for:
--      - Installing a finalizer to generate the bytecodes
--      - Generating the foreign import wrapper.
--      - Splicing in the computation that loads the bytecodes, gets a function pointer through the keyhole, and calls it.
clrQuoteExp
  :: forall language.
     KnownSymbol language
  => Proxy language -> (ClrInlinedGroup language -> IO ClrBytecode) -> String -> Q Exp
clrQuoteExp language clrCompile body = do
  count <- getFinalizerCount @(ClrInlinedUnit language String)
  mod <- thisModule
  stubName <- newName $ printf "%s_stub_%d" (symbolVal language) count
  let (resultType, parsedBody) = parseBody body
  let (antis, parsedBody') = extractArgs toClrArg parsedBody
  let inlinedUnit =
        ClrInlinedExpDetails
          language
          count
          stubName
          (normaliseLineEndings parsedBody')
          antis
          resultType
  pushWrapperGen (clrGenerator language mod clrCompile) $ return (ClrInlinedExp inlinedUnit :: ClrInlinedUnit language String)
  --
  -- splice in a proxy datatype for the late bound class, used to delay the type checking of the stub call
  addTopDecls =<< generateFFIStub inlinedUnit
  --
  -- splice in the bytecode load and call to the stub
  generateClrCall mod inlinedUnit

-- | Quasi quoter for declaration in the clr language.
--   Does not splice anything onto the Haskell source.
clrQuoteDec :: forall language . KnownSymbol language => Proxy language -> (ClrInlinedGroup language -> IO ClrBytecode) -> String -> Q [Dec]
clrQuoteDec language clrCompile body = do
  mod <- thisModule
  pushWrapperGen (clrGenerator language mod clrCompile) $ do
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
    return (ClrInlinedDec language body' :: ClrInlinedUnit language String)
  return mempty


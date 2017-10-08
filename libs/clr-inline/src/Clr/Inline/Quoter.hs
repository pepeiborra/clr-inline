{-# LANGUAGE ViewPatterns #-}
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

import Clr.Host.DriverEntryPoints (unsafeGetPointerToMethod)
import Clr.Host.Method (GetMethodStubDelegate, makeGetMethodStubDelegate)
import Clr.Host.BStr
import Clr.Host.Delegate
import Clr.Host.GCHandle
import Clr.Marshal
import Clr.MarshalF
import Clr.Inline.State
import Clr.Inline.Types
import Clr.Inline.Types.Parse
import Clr.Inline.Types.Quote
import Clr.Inline.Utils.Embed
import Control.Lens
import Control.Monad
import Data.Char
import Data.List
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

data ArgDetails name a
  = Value { _quotedType :: a}
  | Delegate { _delegateName :: name
            ,  _quotedArgs :: [a]
            ,  _quotedReturn :: Maybe a}
makeLenses ''ArgDetails
makePrisms ''ArgDetails

quotes :: Traversal (ArgDetails name a) (ArgDetails name a')  a a'
quotes f (Value q) = Value <$> f q
quotes f (Delegate n aa r) = Delegate n <$> traverse f aa <*> traverse f r

parseArgDetails :: ClrType -> ArgDetails () String
parseArgDetails (Fun [Unit] Unit) = Delegate () [] Nothing
parseArgDetails (Fun [Unit] a) = Delegate () [] (Just $ renderClrType a)
parseArgDetails (Fun args Unit) = Delegate () (map renderClrType args) Nothing
parseArgDetails (Fun args res ) = Delegate () (map renderClrType args) (Just $ renderClrType res)
parseArgDetails other = Value (renderClrType other)

data ClrInlinedExpDetails (language :: Symbol) argType = ClrInlinedExpDetails
  { language :: Proxy language
  , unitId :: Int
  , stubName :: Name
  , body :: String
  , args :: Map String (ArgDetails Name argType)
  , loc  :: Loc
  , returnType :: String
  }

data ClrInlinedUnit (language :: Symbol) argType
  = ClrInlinedExp (ClrInlinedExpDetails language argType)
  | ClrInlinedDec (Proxy language) String

makePrisms ''ClrInlinedUnit
makeLensesFor [("args","_args")] ''ClrInlinedExpDetails

data ClrInlinedGroup language = ClrInlinedGroup
  { mod :: Module
  , units :: [ClrInlinedUnit language ClrTypeSymbol]
  }

getNamespace :: Module -> String
getNamespace (Module (PkgName pkg) _) = printf "Clr.Inline.%s" (mapMaybe escape pkg)
  where
    escape '-' = Just '_'
    escape '.' = Just '_'
    escape x
      | isAlpha x = Just x
      | otherwise = Nothing

getMethodName :: KnownSymbol language => ClrInlinedExpDetails language a -> String
getMethodName ClrInlinedExpDetails{..} = printf "%s_quote_%d" (symbolVal language) unitId

getClassName :: Module -> String
getClassName (Module _ (ModName n)) = map escape n
  where
    escape '.' = '_'
    escape  x  =  x

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
  let resTy = [t| IO $(lookupQuotableMarshalType returnType)|]
      argTy (Value q) = lookupQuotableMarshalType q
      -- The representation of a delegate is GCHandle Int
      argTy Delegate{} = [t| GCHandle Int |]
      mkFunTy = foldrOf folded (\t u -> arrowT `appT` argTy t `appT` u)
  -- This is what we'd like to write:
  -- [d| foreign import ccall "dynamic" $stubName :: $([t|FunPtr $funTy -> $funTy|]) |]
  -- Unfort. splicing languages into foreign import decl is not supported, so we have to write:
  -- TODO Convert every type to its Marshalled counterpart
  ffiStub <- let funTy = mkFunTy resTy args in ForeignD . ImportF CCall Safe "dynamic" stubName <$> [t|FunPtr $funTy -> $funTy|]
  delegateStubs <- forM (toListOf (folded._Delegate) args) $ \(stubName, argTys, resTy) ->
                       let funTy = lookupDelegateMarshalType argTys (maybe [t|()|] lookupQuotableMarshalType resTy)
                       in ForeignD . ImportF CCall Safe "wrapper" stubName <$> [t| $funTy -> IO(FunPtr $funTy) |]
  return $ ffiStub : delegateStubs

getMethodStubRaw :: (GetMethodStubDelegate a)
getMethodStubRaw = unsafeDupablePerformIO $ makeGetMethodStubDelegate <$> unsafeGetPointerToMethod "GetMethodStub"

invoke :: String -> String -> FunPtr a
invoke c m = unsafeDupablePerformIO $ marshal c $ \c -> marshal m $ \m -> return $ getMethodStubRaw c m (BStr nullPtr)

createDelegate :: String -> [String] -> (f -> IO (FunPtr f)) -> IO (f -> IO (GCHandle Int))
createDelegate name types = getDelegateConstructorStub clrtype
  where
    clrtype
      | null types = name
      | otherwise  = printf "%s`%d[%s]" name (length types) (intercalate "," types)

generateClrCall :: KnownSymbol language => Module -> ClrInlinedExpDetails language String -> ExpQ
generateClrCall mod exp@ClrInlinedExpDetails{..} = do
  argsWithDelegates <- iforMOf (itraversed <. _Delegate) args $ \n (stubN,args,res) -> do
      delName <- newName (printf "delegate_%s" n)
      argClrTypes <- mapM (fmap getClrTypeSymbol . lookupQuotableClrType) args
      resClrType <- traverse (fmap getClrTypeSymbol . lookupQuotableClrType) res
      let argCount = case args of ["unit"] -> 0 ; other -> genericLength other
      DoE (init -> stmts) <-
            [| do wrapper <-
                    case resClrType of
                      Nothing -> createDelegate "System.Action" argClrTypes $(varE stubN)
                      Just ty -> createDelegate "System.Func" (argClrTypes ++ [ty]) $(varE stubN)
                  $(varP delName) <- wrapper $ marshalF' (Proxy :: $([t| Proxy $(litT$ numTyLit argCount)|])) $(varE =<< getValueName n)
                  return ()
              |]
      return ((delName, stmts), args, res)
  let argExps =
        [ case argDetails of
            Value{}                              -> [| marshal $(varE =<< getValueName a)|]
            Delegate{_delegateName = (d,_stmts)} -> [| \k -> k $(varE d) |]
        | (a, argDetails) <- Map.toList argsWithDelegates
        ]
      roll m f = [|$m . ($f .)|]
      delegateStmts = concatOf (folded._Delegate._1._2) argsWithDelegates
  DoE (splitAt 3 -> (part1, part2)) <-
      [| do unembedBytecode
            let stub = invoke $(liftString $ getFullClassName language mod) $(liftString $ getMethodName exp)
            let stub_f = $(varE stubName) stub
            result <- $(foldr roll [|id|] argExps) stub_f
            unmarshal result :: IO $(lookupQuotableUnmarshalType returnType)
       |]
  return $ DoE (part1 ++ delegateStmts ++ part2)

marshalF' :: forall (n::Nat) from to . MarshalF n from to => Proxy n -> from -> to
marshalF' Proxy = marshalF @n

-- | Runs after the whole module has been loaded and is responsible for generating:
--     - A clr assembly with all the inline code, embedding it into the module.
clrGenerator
  :: forall language . KnownSymbol language
  => Proxy language -> Module -> (ClrInlinedGroup language -> IO ClrBytecode) -> Q ()
clrGenerator language hsmod compile = do
  FinalizerState {wrappers} <- getFinalizerState @(ClrInlinedUnit language String)
  typedWrappers <-
        mapMOf (traversed . _ClrInlinedExp . _args) (fmap (Map.mapKeysMonotonic toClrArg) . traverseOf (traverse.quotes) lookupQuotableClrType) wrappers
  let mod = ClrInlinedGroup hsmod typedWrappers
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
  let stubString = printf "%s_stub_%d" (symbolVal language) count
  stubName <- newName stubString
  loc <- location
  let ParseResult parsedBody resultType antis = parse toClrArg body
  argDetails <- iforM antis $ \i x -> traverseOf (_Delegate._1) (\_ -> newName $ printf "%s_%s" stubString i) (parseArgDetails x)
  let inlinedUnit =
        ClrInlinedExpDetails
          language
          count
          stubName
          (normaliseLineEndings parsedBody)
          argDetails
          loc
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


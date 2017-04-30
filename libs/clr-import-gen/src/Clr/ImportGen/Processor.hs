{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Clr.ImportGen.Processor where

import Clr

import Clr.Host

import Clr.ImportGen.Definition
import Clr.ImportGen.Reflection

import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax as TH

import qualified Data.Text as T

import Pipes.Prelude(toListM)


clrNameCounter :: IORef Int
{-# NOINLINE clrNameCounter #-}
clrNameCounter = unsafePerformIO (newIORef 0)

newUniqueName :: String -> Q Name
newUniqueName prefix = runIO $ do
  n <- readIORef clrNameCounter
  writeIORef clrNameCounter $ n+1
  return $ mkName $ prefix ++ show n

-- TODO: actually make this a function that can be thread safe
-- so that the CLR is only started once
ensureClrStarted :: Q ()
ensureClrStarted = runIO $ startClr

--
-- A list of each type variable as a TH.Type VarT for each generic parameter of the supplied
-- CLR instance of System.Type. We use the same name to ensure they are matched up again in
-- other instance declarations, but with a prefix to ensure it is lower case.
--
typeToHaskellGenTypVars :: Object T_Type -> Q [TH.Type]
typeToHaskellGenTypVars typ = memberToHaskellGenTypVars $ upCast typ

--
-- A list of each type variable as a TH.Type VarT for each generic parameter of the supplied
-- CLR instance of System.Reflection.MemberInfo. We use the same name to ensure they are
-- matched up again in other instance declarations, but with a prefix to ensure it is lower case.
--
memberToHaskellGenTypVars :: Object T_MemberInfo -> Q [TH.Type]
memberToHaskellGenTypVars mem = do
  genTyps <- runIO $ toListM $ memberGetGenericArguments mem
  names   <- runIO $ mapM typeName genTyps
  let names' = map (\name-> "gt_" `T.append` name) names
  return $ map (VarT . mkName . T.unpack) names'


--
-- The TH.Type (T "name" gt) for the supplied CLR instance of System.Type
--
typeToHaskellRepr :: Object T_Type -> Q TH.Type
typeToHaskellRepr typ = do
  nm <- runIO $ typeFullNm typ >>= return . T.unpack :: Q String
  genVars <- typeToHaskellGenTypVars typ
  return $ ParensT $ ConT (mkName "T") `AppT` (LitT $ StrTyLit nm) `AppT` (ParensT (
    foldr (\a-> \b-> PromotedConsT `AppT` a `AppT` b ) PromotedNilT genVars ))

--
-- The TH.Type (T "name" gt) for the supplied CLR instance of System.Reflection.MemberInfo
--
memberToHaskellRepr :: Object T_MemberInfo -> Q TH.Type
memberToHaskellRepr mem = do
  nm <- runIO $ memberInfoNm mem >>= return . T.unpack :: Q String
  genVars <- memberToHaskellGenTypVars mem
  return $ ParensT $ ConT (mkName "T") `AppT` (LitT $ StrTyLit nm) `AppT` (ParensT (
    foldr (\a-> \b-> PromotedConsT `AppT` a `AppT` b ) PromotedNilT genVars ))

defaultRefs :: [T.Text]
defaultRefs = ["mscorlib"]

defToAssems :: RefImportDef -> Q [Object T_Assembly]
defToAssems def = do
  let refs  = map refToText $ getRefs def
  let refs' = defaultRefs ++ refs
  runIO $ mapM assemblyLoad refs'

defToTypes :: RefImportDef -> Q [Object T_Type]
defToTypes def = do
  assems <- defToAssems def
  let imps = getImps def
  mapM (\assem-> assemGetTypesMatchingImports assem imps) assems >>= return . concat

assemGetTypesMatchingImport :: Object T_Assembly -> Import -> Q [Object T_Type]
assemGetTypesMatchingImport assem (Import ns typs) = case typs of
  [] -> runIO $ toListM $ assemGetAllTypesOfNS assem ns
  xs -> runIO $ toListM $ assemGetTypesByFQName assem (map (\typName -> ns `T.append` T.pack "." `T.append` typName) xs)

assemGetTypesMatchingImports :: Object T_Assembly -> [Import] -> Q [Object T_Type]
assemGetTypesMatchingImports assem imports = mapM (assemGetTypesMatchingImport assem) imports >>= return . concat

declareMembersInstance :: Object T_Type -> [Object T_MemberInfo] -> Q Dec
declareMembersInstance members = undefined

memberDeclareCandidates :: Object T_Type -> [Object T_MemberInfo] -> Q [Dec]
memberDeclareCandidates member = undefined

typeDeclareSupers :: Object T_Type -> Q Dec
typeDeclareSupers typ = undefined

declareType :: Object T_Type -> Q [Dec]
declareType typ = undefined

importDefToQDec :: RefImportDef -> Q [Dec]
importDefToQDec def = do
  ensureClrStarted
  assems <- defToAssems def
  let allImports = getImps def
  typs <- mapM (\assem-> assemGetTypesMatchingImports assem allImports) assems >>= return . concat
  decs <- mapM declareType typs >>= return . concat
  return decs




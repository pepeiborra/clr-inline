{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module Clr.ImportGen.Processor where

import Clr

import Clr.Host

import Clr.ImportGen.Definition
import Clr.ImportGen.Reflection

import Data.Foldable
import Data.Traversable
import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax as TH

import qualified Data.Text as T

import qualified Data.Map.Strict as Map

import Pipes
import qualified Pipes.Prelude as PP(toListM,filterM,mapM)


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
typeToHaskellGenTypVars typ = do
  genTyps <- runIO $ PP.toListM $ typeGetGenericArguments typ
  names   <- runIO $ mapM typeName genTyps
  let names' = map (\name-> "gt_" `T.append` name) names
  return $ map (VarT . mkName . T.unpack) names'

--
-- A list of each type variable as a TH.Type VarT for each generic parameter of the supplied
-- CLR instance of System.Reflection.MemberInfo. We use the same name to ensure they are
-- matched up again in other instance declarations, but with a prefix to ensure it is lower case.
--
memberToHaskellGenTypVars :: Object T_MemberInfo -> Q [TH.Type]
memberToHaskellGenTypVars mem = do
  genTyps <- runIO $ PP.toListM $ memberGetGenericArguments mem
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

candidateArgsToHaskellRepr :: Object T_MethodBase -> Q TH.Type
candidateArgsToHaskellRepr method = do
  paramTypes <- runIO $ PP.toListM $ methodGetParameters method >-> PP.mapM parameterInfoParameterType
  paramTypeReps <- mapM typeToHaskellRepr paramTypes
  return $ ParensT (
    foldr (\a-> \b-> PromotedConsT `AppT` a `AppT` b ) PromotedNilT paramTypeReps )

candidatesArgsToHaskellReprs :: [Object T_MethodBase] -> Q TH.Type
candidatesArgsToHaskellReprs methods = do
  candidates <- mapM candidateArgsToHaskellRepr methods
  return $ ParensT (
    foldr (\a-> \b-> PromotedConsT `AppT` a `AppT` b ) PromotedNilT candidates )

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
  [] -> runIO $ PP.toListM $ assemGetAllTypesOfNS assem ns
  xs -> runIO $ PP.toListM $ assemGetTypesByFQName assem (map (\typName -> ns `T.append` T.pack "." `T.append` typName) xs)

assemGetTypesMatchingImports :: Object T_Assembly -> [Import] -> Q [Object T_Type]
assemGetTypesMatchingImports assem imports = mapM (assemGetTypesMatchingImport assem) imports >>= return . concat

membersToRepMap :: [Object T_MemberInfo] -> Q (Map.Map TH.Type [Object T_MemberInfo])
membersToRepMap = foldlM updateMap Map.empty
  where updateMap :: Map.Map TH.Type [Object T_MemberInfo] -> Object T_MemberInfo -> Q (Map.Map TH.Type [Object T_MemberInfo])
        updateMap mp member = do
          rep <- memberToHaskellRepr member
          return $ Map.insertWith (++) rep [member] mp

methodsToRepMap :: [Object T_MethodBase] -> Q (Map.Map TH.Type [Object T_MethodBase])
methodsToRepMap = foldlM updateMap Map.empty
  where updateMap :: Map.Map TH.Type [Object T_MethodBase] -> Object T_MethodBase -> Q (Map.Map TH.Type [Object T_MethodBase])
        updateMap mp member = do
          rep <- memberToHaskellRepr $ upCast member
          return $ Map.insertWith (++) rep [member] mp

memberIsNotSpecial :: Object T_MemberInfo -> IO Bool
memberIsNotSpecial mem = do
  name <- memberInfoName mem
  let isGetter   = "get_"    `T.isPrefixOf` name
  let isSetter   = "set_"    `T.isPrefixOf` name
  let isAdder    = "add_"    `T.isPrefixOf` name
  let isRemover  = "remove_" `T.isPrefixOf` name
  return $ not isGetter && not isSetter && not isAdder && not isRemover

typeDeclareMembers :: Object T_Type -> Q Dec
typeDeclareMembers typ = do
  typRep <- typeToHaskellRepr typ
  members <- runIO $ PP.toListM $ typeGetMembers typ >-> PP.filterM memberIsNotSpecial
  memberByRep <- membersToRepMap members
  return $ TySynInstD (mkName "Members") (TySynEqn [typRep] $ Map.foldrWithKey (\memRep-> \mem-> \xs-> memRep `AppT` PromotedConsT `AppT` xs) PromotedNilT memberByRep)

typeDeclareCandidates :: Object T_Type -> Q [Dec]
typeDeclareCandidates typ = do
  typRep <- typeToHaskellRepr typ
  members <- runIO $ PP.toListM $ typeGetMembers typ >-> PP.filterM memberIsNotSpecial
  methods <- runIO $ membersToMethodBases members
  methodByRep <- methodsToRepMap methods
  let candidatesByMethodRep = Map.map candidatesArgsToHaskellReprs methodByRep
  mapM (\(m,qc)-> qc >>= \c-> return $ TySynInstD (mkName "Candidates") (TySynEqn [typRep, m] c)) $ Map.toList candidatesByMethodRep

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




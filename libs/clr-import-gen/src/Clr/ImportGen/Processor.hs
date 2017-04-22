{-# LANGUAGE TemplateHaskell #-}

module Clr.ImportGen.Processor where

import Clr

import Clr.Host

import Clr.ImportGen.Definition
import Clr.ImportGen.Reflection

import Data.IORef
import System.IO.Unsafe(unsafePerformIO)

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

import qualified Data.Text as T

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

defToAssems :: RefImportDef -> Q [Object T_Assembly]
defToAssems = undefined

assemGetTypesByFQName' :: Object T_Assembly -> [T.Text] -> Q [Object T_Type]
assemGetTypesByFQName' = undefined

assemGetAllTypesOfNS' = undefined

assemGetTypesMatchingImport :: Object T_Assembly -> Import -> Q [Object T_Type]
assemGetTypesMatchingImport assem (Import ns typs) = case typs of
  [] -> assemGetAllTypesOfNS'  assem ns
  xs -> assemGetTypesByFQName' assem (map (\typName -> ns `T.append` T.pack "." `T.append` typName) xs)

assemGetTypesMatchingImports :: Object T_Assembly -> [Import] -> Q [Object T_Type]
assemGetTypesMatchingImports assem imports = mapM (assemGetTypesMatchingImport assem) imports >>= return . concat

declareMembersInstance :: [Object T_MemberInfo] -> Q Dec
declareMembersInstance members = undefined

memberDeclareCandidates :: Object T_MemberInfo -> Q [Dec]
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




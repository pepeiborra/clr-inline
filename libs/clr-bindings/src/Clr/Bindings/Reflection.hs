{-# LANGUAGE TypeApplications, TypeInType, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}

module Clr.Bindings.Reflection where

import Clr
import Clr.TypeString

import Clr.Host
import Clr.Host.BStr

import Clr.Bindings.DynImports
import Clr.Bindings.Host
import Clr.Bindings.IEnumerable
import Clr.Bindings.Object

import Control.Monad(filterM)
import Data.Word
import Foreign.Ptr
import qualified Data.Text as T

type T_Assembly      = T "System.Reflection.Assembly" '[]
type T_AssemblyArray = T "System.Reflection.Assembly[]" '[]
type T_AppDomain     = T "System.AppDomain" '[]
type T_Type          = T "System.Type" '[]
type T_TypeArray     = T "System.Type[]" '[]

type T_Load          = T "Load" '[]
type T_CurrentDomain = T "CurrentDomain" '[]
type T_GetAssemblies = T "GetAssemblies" '[]
type T_GetTypes      = T "GetTypes" '[]
type T_FullName      = T "FullName" '[]

type instance Members T_AppDomain = '[ T_CurrentDomain, T_GetAssemblies ]
type instance Members T_Assembly  = '[ T_GetTypes, T_Load ]
type instance Members T_Type      = '[ T_FullName ]

type instance Candidates T_AppDomain T_GetAssemblies = '[ '[ ] ]
type instance Candidates T_Assembly  T_GetTypes      = '[ '[ ] ]
type instance Candidates T_Assembly  T_Load          = '[ '[ T_string ] ]

type instance SuperTypes T_AppDomain     = '[ T_object ]
type instance SuperTypes T_Assembly      = '[ T_object ]
type instance SuperTypes T_AssemblyArray = '[ T_IEnumerable T_Assembly ]
type instance SuperTypes T_Type          = '[ T_object ]
type instance SuperTypes T_TypeArray     = '[ T_IEnumerable T_Type ]

foreign import ccall "dynamic" makeAppDomainCurrentDomain :: FunPtr (IO (ObjectID T_AppDomain)) -> IO (ObjectID T_AppDomain)
foreign import ccall "dynamic" makeAppDomainGetAssemblies :: FunPtr (ObjectID T_AppDomain -> IO (ObjectID T_AssemblyArray)) -> (ObjectID T_AppDomain -> IO (ObjectID T_AssemblyArray))
foreign import ccall "dynamic" makeAssemblyGetTypes       :: FunPtr (ObjectID T_Assembly -> IO (ObjectID T_TypeArray)) -> (ObjectID T_Assembly -> IO (ObjectID T_TypeArray))
foreign import ccall "dynamic" makeAssemblyLoad           :: FunPtr (BStr -> IO (ObjectID T_Assembly)) -> (BStr -> IO (ObjectID T_Assembly))
foreign import ccall "dynamic" makeTypeFullName           :: FunPtr (ObjectID T_Type -> IO BStr) -> (ObjectID T_Type -> IO BStr)

instance PropertyS T_AppDomain T_CurrentDomain where
  type PropertyTypeS T_AppDomain T_CurrentDomain = T_AppDomain

instance PropertyDynImportGetS T_AppDomain T_CurrentDomain where
  propertyDynImportGetS = makeAppDomainCurrentDomain

instance MethodResultI1 T_AppDomain T_GetAssemblies () where
  type ResultTypeI1 T_AppDomain T_GetAssemblies () = 'Just T_AssemblyArray

instance MethodDynImportI1 T_AppDomain T_GetAssemblies () where
  methodDynImportI1 = makeAppDomainGetAssemblies

instance MethodResultI1 T_Assembly T_GetTypes () where
  type ResultTypeI1 T_Assembly T_GetTypes () = 'Just T_TypeArray

instance MethodDynImportI1 T_Assembly T_GetTypes () where
  methodDynImportI1 = makeAssemblyGetTypes

instance MethodResultS1 T_Assembly T_Load T_string where
  type ResultTypeS1 T_Assembly T_Load T_string = 'Just T_Assembly

instance MethodDynImportS1 T_Assembly T_Load T_string where
  methodDynImportS1 = makeAssemblyLoad

instance PropertyI T_Type T_FullName where
  type PropertyTypeI T_Type T_FullName = T_string

instance PropertyDynImportGetI T_Type T_FullName where
  propertyDynImportGetI = makeTypeFullName

currentDomain :: IO (Object T_AppDomain)
currentDomain = getPropS @T_CurrentDomain @T_AppDomain

assemblyLoad :: T.Text -> IO (Object T_Assembly)
assemblyLoad assemName = invokeS @T_Load @T_Assembly assemName

assemIsDynamicDriverInternal :: Object T_Assembly -> IO Bool
assemIsDynamicDriverInternal assem = invokeI @T_ToString assem () >>= \assemName-> return $ assemName == "DynamicAssembly, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null"

assembliesLoaded :: IO [Object T_Assembly]
assembliesLoaded = do
  assems <- assembliesLoaded'
  filterM (\assem-> assemIsDynamicDriverInternal assem >>= return . not) assems

assembliesLoaded' :: IO [Object T_Assembly]
assembliesLoaded'  = do
  domain <- currentDomain
  assems <- invokeI @T_GetAssemblies domain ()
  toListM assems

assemGetTypes :: Object T_Assembly -> IO [Object T_Type]
assemGetTypes assem = invokeI @T_GetTypes assem () >>= toListM

knownTypes :: IO [Object T_Type]
knownTypes = do
  assems <- assembliesLoaded
  types  <- mapM assemGetTypes assems
  return $ concat types

typeFullName :: Object T_Type -> IO String
typeFullName typ = getPropI @T_FullName typ


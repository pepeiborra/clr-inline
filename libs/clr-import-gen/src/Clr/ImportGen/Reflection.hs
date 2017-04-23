{-# LANGUAGE TypeApplications, TypeInType, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, OverloadedStrings #-}

module Clr.ImportGen.Reflection where

import Prelude hiding (concat, mapM, sequence)

import Clr
import Clr.TypeString

import Clr.Host
import Clr.Host.BStr

import Clr.Bindings.DynImports
import Clr.Bindings.IEnumerable
import Clr.Bindings.Object

import Clr.Marshal.Host

import Control.Monad(forM_)
import Data.Word
import Foreign.Ptr

import qualified Data.Text as T

import Pipes
import Pipes.Prelude

type T_Assembly        = T "System.Reflection.Assembly" '[]
type T_AssemblyArray   = T "System.Reflection.Assembly[]" '[]
type T_AppDomain       = T "System.AppDomain" '[]
type T_Type            = T "System.Type" '[]
type T_TypeArray       = T "System.Type[]" '[]
type T_MemberInfo      = T "System.Reflection.MemberInfo" '[]
type T_MemberInfoArray = T "System.Reflection.MemberInfo[]" '[]

type T_Load                = T "Load" '[]
type T_CurrentDomain       = T "CurrentDomain" '[]
type T_GetAssemblies       = T "GetAssemblies" '[]
type T_GetTypes            = T "GetTypes" '[]
type T_FullName            = T "FullName" '[]
type T_GetMembers          = T "GetMembers" '[]
type T_Name                = T "Name" '[]
type T_Namespace           = T "Namespace" '[]
type T_GetGenericArguments = T "GetGenericArguments" '[]
type T_GetType             = T "GetType" '[]

type instance Members T_AppDomain  = '[ T_CurrentDomain, T_GetAssemblies ]
type instance Members T_Assembly   = '[ T_GetTypes, T_Load, T_GetType ]
type instance Members T_Type       = '[ T_FullName, T_GetMembers, T_Namespace, T_GetGenericArguments ]
type instance Members T_MemberInfo = '[ T_Name ]

type instance Candidates T_AppDomain T_GetAssemblies       = '[ '[ ] ]
type instance Candidates T_Assembly  T_GetTypes            = '[ '[ ] ]
type instance Candidates T_Assembly  T_Load                = '[ '[ T_string ] ]
type instance Candidates T_Type      T_GetMembers          = '[ '[ ] ]
type instance Candidates T_Type      T_GetGenericArguments = '[ '[ ] ]
type instance Candidates T_Assembly  T_GetType             = '[ '[ T_string ] ]

type instance SuperTypes T_AppDomain       = '[ T_object ]
type instance SuperTypes T_Assembly        = '[ T_object ]
type instance SuperTypes T_AssemblyArray   = '[ T_IEnumerable T_Assembly ]
type instance SuperTypes T_Type            = '[ T_MemberInfo ]
type instance SuperTypes T_TypeArray       = '[ T_IEnumerable T_Type ]
type instance SuperTypes T_MemberInfo      = '[ T_object ]
type instance SuperTypes T_MemberInfoArray = '[ T_IEnumerable T_MemberInfo ]

foreign import ccall "dynamic" makeAppDomainCurrentDomain  :: FunPtr (IO (ObjectID T_AppDomain)) -> IO (ObjectID T_AppDomain)
foreign import ccall "dynamic" makeAppDomainGetAssemblies  :: FunPtr (ObjectID T_AppDomain -> IO (ObjectID T_AssemblyArray)) -> (ObjectID T_AppDomain -> IO (ObjectID T_AssemblyArray))
foreign import ccall "dynamic" makeAssemblyGetTypes        :: FunPtr (ObjectID T_Assembly -> IO (ObjectID T_TypeArray)) -> (ObjectID T_Assembly -> IO (ObjectID T_TypeArray))
foreign import ccall "dynamic" makeAssemblyLoad            :: FunPtr (BStr -> IO (ObjectID T_Assembly)) -> (BStr -> IO (ObjectID T_Assembly))
foreign import ccall "dynamic" makeTypeFullName            :: FunPtr (ObjectID T_Type -> IO BStr) -> (ObjectID T_Type -> IO BStr)
foreign import ccall "dynamic" makeTypeGetMembers          :: FunPtr (ObjectID T_Type -> IO (ObjectID T_MemberInfoArray)) -> (ObjectID T_Type -> IO (ObjectID T_MemberInfoArray))
foreign import ccall "dynamic" makeMemberInfoName          :: FunPtr (ObjectID T_MemberInfo -> IO BStr) -> (ObjectID T_MemberInfo -> IO BStr)
foreign import ccall "dynamic" makeTypeNamespace           :: FunPtr (ObjectID T_Type -> IO BStr) -> (ObjectID T_Type -> IO BStr)
foreign import ccall "dynamic" makeTypeGetGenericArguments :: FunPtr (ObjectID T_Type -> IO (ObjectID T_TypeArray)) -> (ObjectID T_Type -> IO (ObjectID T_TypeArray))
foreign import ccall "dynamic" makeAssemblyGetType         :: FunPtr (ObjectID T_Assembly -> BStr -> IO (ObjectID T_Type)) -> (ObjectID T_Assembly -> BStr -> IO (ObjectID T_Type))

instance PropertyS T_AppDomain T_CurrentDomain where
  type PropertyTypeS T_AppDomain T_CurrentDomain = T_AppDomain

instance MethodResultI1 T_AppDomain T_GetAssemblies () where
  type ResultTypeI1 T_AppDomain T_GetAssemblies () = 'Just T_AssemblyArray

instance MethodResultI1 T_Assembly T_GetTypes () where
  type ResultTypeI1 T_Assembly T_GetTypes () = 'Just T_TypeArray

instance MethodResultS1 T_Assembly T_Load T_string where
  type ResultTypeS1 T_Assembly T_Load T_string = 'Just T_Assembly

instance PropertyI T_Type T_FullName where
  type PropertyTypeI T_Type T_FullName = T_string

instance MethodResultI1 T_Type T_GetMembers () where
  type ResultTypeI1 T_Type T_GetMembers () = 'Just T_MemberInfoArray

instance PropertyI T_MemberInfo T_Name where
  type PropertyTypeI T_MemberInfo T_Name = T_string

instance PropertyI T_Type T_Namespace where
  type PropertyTypeI T_Type T_Namespace = T_string

instance MethodResultI1 T_Type T_GetGenericArguments () where
  type ResultTypeI1 T_Type T_GetGenericArguments () = 'Just T_TypeArray

instance MethodResultI1 T_Assembly T_GetType T_string where
  type ResultTypeI1 T_Assembly T_GetType T_string = 'Just T_Type

instance PropertyDynImportGetS T_AppDomain T_CurrentDomain where
  propertyDynImportGetS = makeAppDomainCurrentDomain

instance MethodDynImportI1 T_Assembly T_GetTypes () where
  methodDynImportI1 = makeAssemblyGetTypes

instance MethodDynImportI1 T_AppDomain T_GetAssemblies () where
  methodDynImportI1 = makeAppDomainGetAssemblies

instance MethodDynImportS1 T_Assembly T_Load T_string where
  methodDynImportS1 = makeAssemblyLoad

instance PropertyDynImportGetI T_Type T_FullName where
  propertyDynImportGetI = makeTypeFullName

instance MethodDynImportI1 T_Type T_GetMembers () where
  methodDynImportI1 = makeTypeGetMembers

instance PropertyDynImportGetI T_MemberInfo T_Name where
  propertyDynImportGetI = makeMemberInfoName

instance PropertyDynImportGetI T_Type T_Namespace where
  propertyDynImportGetI = makeTypeNamespace

instance MethodDynImportI1 T_Type T_GetGenericArguments () where
  methodDynImportI1 = makeTypeGetGenericArguments

instance MethodDynImportI1 T_Assembly T_GetType T_string where
  methodDynImportI1 = makeAssemblyGetType


--
-- AppDomain.CurrentDomain
--
currentDomain :: IO (Object T_AppDomain)
currentDomain = getPropS @T_CurrentDomain @T_AppDomain

--
-- Assembly.Load(System.String)
--
assemblyLoad :: T.Text -> IO (Object T_Assembly)
assemblyLoad assemName = invokeS @T_Load @T_Assembly assemName

--
-- assemIsDynamicDriverInternal assem is true if the assembly represents that created on the fly by the driver
--
assemIsDynamicDriverInternal :: Object T_Assembly -> IO Bool
assemIsDynamicDriverInternal assem = invokeI @T_ToString assem () >>= \assemName-> return $ assemName == "DynamicAssembly, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null"

--
-- assembliesLoaded is a producer of the currently loaded assemblies minus the internal one created by the Driver
--
assembliesLoaded :: Producer (Object T_Assembly) IO ()
assembliesLoaded = assembliesLoaded' >-> filterM (\assem-> assemIsDynamicDriverInternal assem >>= return . not)

--
-- assembliesLoaded' is a producer of the currently loaded assemblies
--
assembliesLoaded' :: Producer (Object T_Assembly) IO ()
assembliesLoaded' = do
  domain <- liftIO $ currentDomain
  assems <- liftIO $ invokeI @T_GetAssemblies domain ()
  toProducer assems

--
-- assemGetTypes assem is a producer of types defined within assem
--
assemGetTypes :: Object T_Assembly -> Producer (Object T_Type) IO ()
assemGetTypes assem = do
  typs <- liftIO $ invokeI @T_GetTypes assem ()
  toProducer typs

--
-- knownTypes is a producer of all the types founds in all the currently loaded assemblies
--
knownTypes :: Producer (Object T_Type) IO ()
knownTypes = for assembliesLoaded assemGetTypes

--
-- System.Type.FullName
--
typeFullName :: Object T_Type -> IO T.Text
typeFullName typ = getPropI @T_FullName typ

--
-- System.Type.GetMembers()
--
typeGetMembers :: Object T_Type -> Producer (Object T_MemberInfo) IO ()
typeGetMembers typ = liftIO (invokeI @T_GetMembers typ ()) >>= toProducer

--
-- typeName is System.Reflection.MemberInfo.Name on a parameter of System.Type
--
-- NB: This highlights a current issue where we'd actually like to syntacticaly declare
-- something that is compatible with the type as simple you do in C#, but in Haskell calling
-- memberInfoName on a type will result in a compilation error of could not match...
-- The other end of the spectrum is that of toProducer within the IEnumerable module, but
-- the signature is bit complicated and not easy to deduce, polymorphism can cause other
-- compilation errors. TODO.
typeName :: Object T_Type -> IO T.Text
typeName typ = getPropI @T_Name typ

--
-- System.Type.Namespace
--
typeNamespace :: Object T_Type -> IO T.Text
typeNamespace typ = getPropI @T_Namespace typ

--
-- System.Reflection.MemberInfo.Name
--
memberInfoName :: Object T_MemberInfo -> IO T.Text
memberInfoName mi = getPropI @T_Name mi

--
-- assemGetTypesOfNs assem ns, is each type within assem that has a matching namespace of ns
--
assemGetAllTypesOfNS :: Object T_Assembly -> T.Text -> Producer (Object T_Type) IO ()
assemGetAllTypesOfNS assem ns = assemGetTypes assem >-> filterM (\typ-> typeNamespace typ >>= \nsTyp-> return $ nsTyp == ns)

--
-- simplifyTypeName returns the supplied string upto but not including the "`"
--
simplifyTypeName :: T.Text -> T.Text
simplifyTypeName = fst . T.breakOn "`"

--
-- typeFullNm is like typeFullName, except simplifyTypeName is called on the result
--
typeFullNm :: Object T_Type -> IO T.Text
typeFullNm typ = typeFullName typ >>= return . simplifyTypeName

--
-- typeIsSupported returns false if the name contains a particular character
-- that we'd like to ignore at least for an early release
--
typeIsSupported :: Object T_Type -> IO Bool
typeIsSupported typ = do
  name <- typeFullName typ
  let containsUnderscore   = "_" `T.isInfixOf` name -- Internal only? Generally can ignore these.
  let containsAngleBracket = "<" `T.isInfixOf` name -- These occur when a nested class depends on the generic type instantiation of the parent. Ignore these for now then revisit. TODO.
  let containsPlus         = "+" `T.isInfixOf` name -- Probably need further work to support nested classes in general. TODO.
  return $ not containsUnderscore && not containsAngleBracket && not containsPlus

--
-- System.Type.GetGenericArguments()
--
typeGetGenericArguments :: Object T_Type -> Producer (Object T_Type) IO ()
typeGetGenericArguments typ = liftIO (invokeI @T_GetGenericArguments typ ()) >>= toProducer

--
-- System.Reflection.Assembly.GetType(System.String)
--
assemGetType :: Object T_Assembly -> T.Text -> IO (Object T_Type)
assemGetType = invokeI @T_GetType

assemGetTypesByFQName :: Object T_Assembly -> [T.Text] -> Producer (Object T_Type) IO ()
assemGetTypesByFQName assem names = forM_ names $ \name-> do
    typ <- liftIO $ assemGetType assem name
    yield typ


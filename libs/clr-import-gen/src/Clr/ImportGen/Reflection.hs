{-# LANGUAGE TypeApplications, TypeInType, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, OverloadedStrings, ScopedTypeVariables, TypeOperators, AllowAmbiguousTypes #-}

module Clr.ImportGen.Reflection where

import Prelude hiding (concat, mapM, sequence)

import Clr
import Clr.Resolver
import Clr.TypeString

import Clr.Host
import Clr.Host.BStr
import Clr.Host.GCHandle

import Clr.Bindings.DynImports
import Clr.Bindings.IEnumerable
import Clr.Bindings.Object

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
type T_MethodInfo      = T "System.Reflection.MethodInfo" '[]

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
type T_IsAssignableFrom    = T "IsAssignableFrom" '[]

type instance Members T_AppDomain  = '[ T_CurrentDomain, T_GetAssemblies ]
type instance Members T_Assembly   = '[ T_GetTypes, T_Load, T_GetType ]
type instance Members T_MemberInfo = '[ T_Name ]
type instance Members T_MethodInfo = '[ T_GetGenericArguments ]

type instance Candidates T_AppDomain  T_GetAssemblies       = '[ '[ ] ]
type instance Candidates T_Assembly   T_GetTypes            = '[ '[ ] ]
type instance Candidates T_Assembly   T_Load                = '[ '[ T_string ] ]
type instance Candidates T_Type       T_GetMembers          = '[ '[ ] ]
type instance Candidates T_Type       T_GetGenericArguments = '[ '[ ] ]
type instance Candidates T_Assembly   T_GetType             = '[ '[ T_string ] ]
type instance Candidates T_Type       T_IsAssignableFrom    = '[ '[ T_Type ] ]
type instance Candidates T_Type       T_GetType             = '[ '[ T_string ] ]
type instance Candidates T_MethodInfo T_GetGenericArguments = '[ '[ ] ]

type instance SuperTypes T_AppDomain       = '[ T_object ]
type instance SuperTypes T_Assembly        = '[ T_object ]
type instance SuperTypes T_AssemblyArray   = '[ T_IEnumerable T_Assembly ]
type instance SuperTypes T_Type            = '[ T_MemberInfo ]
type instance SuperTypes T_TypeArray       = '[ T_IEnumerable T_Type ]
type instance SuperTypes T_MemberInfo      = '[ T_object ]
type instance SuperTypes T_MemberInfoArray = '[ T_IEnumerable T_MemberInfo ]
type instance SuperTypes T_MethodInfo      = '[ T_MemberInfo ]

foreign import ccall "dynamic" makeAppDomainCurrentDomain    :: FunPtr (IO (GCHandle T_AppDomain)) -> IO (GCHandle T_AppDomain)
foreign import ccall "dynamic" makeAppDomainGetAssemblies    :: FunPtr (GCHandle T_AppDomain -> IO (GCHandle T_AssemblyArray)) -> (GCHandle T_AppDomain -> IO (GCHandle T_AssemblyArray))
foreign import ccall "dynamic" makeAssemblyGetTypes          :: FunPtr (GCHandle T_Assembly -> IO (GCHandle T_TypeArray)) -> (GCHandle T_Assembly -> IO (GCHandle T_TypeArray))
foreign import ccall "dynamic" makeAssemblyLoad              :: FunPtr (BStr -> IO (GCHandle T_Assembly)) -> (BStr -> IO (GCHandle T_Assembly))
foreign import ccall "dynamic" makeTypeFullName              :: FunPtr (GCHandle T_Type -> IO BStr) -> (GCHandle T_Type -> IO BStr)
foreign import ccall "dynamic" makeTypeGetMembers            :: FunPtr (GCHandle T_Type -> IO (GCHandle T_MemberInfoArray)) -> (GCHandle T_Type -> IO (GCHandle T_MemberInfoArray))
foreign import ccall "dynamic" makeMemberInfoName            :: FunPtr (GCHandle T_MemberInfo -> IO BStr) -> (GCHandle T_MemberInfo -> IO BStr)
foreign import ccall "dynamic" makeTypeNamespace             :: FunPtr (GCHandle T_Type -> IO BStr) -> (GCHandle T_Type -> IO BStr)
foreign import ccall "dynamic" makeTypeGetGenericArguments   :: FunPtr (GCHandle T_Type -> IO (GCHandle T_TypeArray)) -> (GCHandle T_Type -> IO (GCHandle T_TypeArray))
foreign import ccall "dynamic" makeAssemblyGetType           :: FunPtr (GCHandle T_Assembly -> BStr -> IO (GCHandle T_Type)) -> (GCHandle T_Assembly -> BStr -> IO (GCHandle T_Type))
foreign import ccall "dynamic" makeMethodGetGenericArguments :: FunPtr (GCHandle T_MethodInfo -> IO (GCHandle T_TypeArray)) -> (GCHandle T_MethodInfo -> IO (GCHandle T_TypeArray))

instance PropertyS T_AppDomain T_CurrentDomain where
  type PropertyTypeS T_AppDomain T_CurrentDomain = T_AppDomain

instance MethodResultI1 T_AppDomain T_GetAssemblies () where
  type ResultTypeI1 T_AppDomain T_GetAssemblies () = T_AssemblyArray

instance MethodResultI1 T_Assembly T_GetTypes () where
  type ResultTypeI1 T_Assembly T_GetTypes () = T_TypeArray

instance MethodResultS1 T_Assembly T_Load T_string where
  type ResultTypeS1 T_Assembly T_Load T_string = T_Assembly

instance PropertyI T_Type T_FullName where
  type PropertyTypeI T_Type T_FullName = T_string

instance MethodResultI1 T_Type T_GetMembers () where
  type ResultTypeI1 T_Type T_GetMembers () = T_MemberInfoArray

instance PropertyI T_MemberInfo T_Name where
  type PropertyTypeI T_MemberInfo T_Name = T_string

instance PropertyI T_Type T_Namespace where
  type PropertyTypeI T_Type T_Namespace = T_string

instance MethodResultI1 T_Type T_GetGenericArguments () where
  type ResultTypeI1 T_Type T_GetGenericArguments () = T_TypeArray

instance MethodResultI1 T_Assembly T_GetType T_string where
  type ResultTypeI1 T_Assembly T_GetType T_string = T_Type

instance MethodResultI1 T_MethodInfo T_GetGenericArguments () where
  type ResultTypeI1 T_MethodInfo T_GetGenericArguments () = T_TypeArray

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

instance MethodDynImportI1 T_MethodInfo T_GetGenericArguments () where
  methodDynImportI1 = makeMethodGetGenericArguments

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
assemIsDynamicDriverInternal assem = objectToString assem >>= \assemName-> return $ assemName == "DynamicAssembly, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null"

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
  assems <- liftIO $ (invokeI @T_GetAssemblies domain () :: IO (Object T_AssemblyArray))
  toProducer assems

--
-- assemGetTypes assem is a producer of types defined within assem
--
assemGetTypes :: Object T_Assembly -> Producer (Object T_Type) IO ()
assemGetTypes assem = do
  typs <- liftIO $ (invokeI @T_GetTypes assem () :: IO (Object T_TypeArray))
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
typeGetMembers typ = liftIO (invokeI @T_GetMembers typ () :: IO (Object T_MemberInfoArray)) >>= toProducer

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
-- memberInfoNm is like memberInfoName, except simplifyTypeName is called on the result
--
memberInfoNm :: Object T_MemberInfo -> IO T.Text
memberInfoNm mi = getPropI @T_Name mi >>= return . simplifyTypeName

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
typeGetGenericArguments typ = liftIO (invokeI @T_GetGenericArguments typ () :: IO (Object T_TypeArray)) >>= toProducer

--
-- System.Reflection.Assembly.GetType(System.String)
--
assemGetType :: Object T_Assembly -> T.Text -> IO (Object T_Type)
assemGetType = invokeI @T_GetType

assemGetTypesByFQName :: Object T_Assembly -> [T.Text] -> Producer (Object T_Type) IO ()
assemGetTypesByFQName assem names = forM_ names $ \name-> do
    typ <- liftIO $ assemGetType assem name
    yield typ

--
-- System.Type.IsAssignableFrom(System.Type)
--
typeIsAssignableFrom :: Object T_Type -> Object T_Type -> IO (Bool)
typeIsAssignableFrom t1 t2 = invokeI @T_IsAssignableFrom t1 t2

--
-- System.Type.GetType(System.String)
--
typeGetType :: T.Text -> IO (Object T_Type)
typeGetType = invokeS @T_GetType @T_Type

--
-- System.Reflection.MethodInfo.GetGenericArguments()
--
methodGetGenericArguments :: Object T_MethodInfo -> Producer (Object T_Type) IO ()
methodGetGenericArguments mth = liftIO (invokeI @T_GetGenericArguments mth () :: IO (Object T_TypeArray)) >>= toProducer

memberGetGenericArguments :: Object T_MemberInfo -> Producer (Object T_Type) IO ()
memberGetGenericArguments mem = do
  method <- liftIO $ downCast @T_MethodInfo mem
  case method of
    Just m  -> methodGetGenericArguments m
    Nothing -> return ()

getmscorlib :: IO (Object T_Assembly)
getmscorlib = assemblyLoad "mscorlib"


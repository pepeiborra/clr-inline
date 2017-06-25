{-# LANGUAGE TypeInType, TypeApplications, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Clr.Bindings.Object.Internal
  ( typeGetType
  , typeIsAssignableFrom
  , T_Type
  , T_ToString
  , T_GetType
  ) where

import Clr
import Clr.Bridge
import Clr.Resolver
import Clr.TypeString

import Clr.Marshal

import Clr.Host
import Clr.Host.BStr
import Clr.Host.GCHandle

import Clr.Bindings.DynImports
import Clr.Bindings.BStr

import Data.Coerce
import Foreign.Ptr
import Foreign.ForeignPtr

import Data.Text as T

--
-- Synonyms for other types referenced here
--
type T_Type = T "System.Type" '[]

--
-- Object methods
--
type T_Equals              = T "Equals" '[]
type T_Finalize            = T "Finalize" '[]
type T_GetHashCode         = T "GetHashCode" '[]
type T_MemberwiseClone     = T "MemberwiseClone" '[]
type ReferenceEquals       = T "ReferenceEquals" '[]
type T_ToString            = T "ToString" '[]

--
-- Type members
--
type T_BaseType            = T "BaseType" '[]
type T_FullName            = T "FullName" '[]
type T_GetGenericArguments = T "GetGenericArguments" '[]
type T_GetInterfaces       = T "GetInterfaces" '[]
type T_GetMembers          = T "GetMembers" '[]
type T_IsAssignableFrom    = T "IsAssignableFrom" '[]
type T_Namespace           = T "Namespace" '[]

-- Members that share the same name on both Object & Type
type T_GetType             = T "GetType"  '[]

type instance SuperTypes T_object = '[ ]

--
-- Member instance declarations. Only are few are used internally, but the others must be declared also
-- as Haskell always exports & imports instances. The import generator must then skip over these.
--
type instance Members T_object = '[ T_ToString, T_GetType ]

-- Doesn't yet conver all members of System.Type. TODO
type instance Members T_Type   = '[ T_BaseType
                                  , T_FullName
                                  , T_GetGenericArguments
                                  , T_GetInterfaces
                                  , T_GetMembers
                                  , T_GetType
                                  , T_IsAssignableFrom
                                  , T_Namespace              ]

type instance Candidates T_object T_ToString          = '[ '[ ] ]
type instance Candidates T_object T_GetType           = '[ '[ ] ]
type instance Candidates T_Type   T_GetType           = '[ '[ T_string ] ]
type instance Candidates T_Type   T_IsAssignableFrom  = '[ '[ T_Type ] ]

foreign import ccall "dynamic" makeObjToString          :: FunPtr (GCHandle t -> IO BStr) -> (GCHandle t -> IO BStr)
foreign import ccall "dynamic" makeObjGetType           :: FunPtr (GCHandle t -> IO (GCHandle (T "System.Type" '[]))) -> (GCHandle t -> IO (GCHandle (T "System.Type" '[])))
foreign import ccall "dynamic" makeTypeGetType          :: FunPtr (BStr -> IO (GCHandle T_Type)) -> (BStr -> IO (GCHandle T_Type))
foreign import ccall "dynamic" makeTypeIsAssignableFrom :: FunPtr (GCHandle T_Type -> GCHandle T_Type -> IO Bool) -> (GCHandle T_Type -> GCHandle T_Type -> IO Bool)

instance MethodResultI1 T_object T_ToString () where
  type ResultTypeI1 T_object T_ToString () = T_string

instance MethodResultI1 T_object T_GetType () where
  type ResultTypeI1 T_object T_GetType () = T_Type

instance MethodResultS1 T_Type T_GetType T_string where
  type ResultTypeS1 T_Type T_GetType T_string = T_Type

instance MethodResultI1 T_Type T_IsAssignableFrom T_Type where
  type ResultTypeI1 T_Type T_IsAssignableFrom T_Type = T_bool

instance MethodDynImportI1 T_object T_ToString () where
  methodDynImportI1 = makeObjToString

instance MethodDynImportI1 T_object T_GetType () where
  methodDynImportI1 = makeObjGetType

instance MethodDynImportS1 T_Type T_GetType T_string where
  methodDynImportS1 = makeTypeGetType

instance MethodDynImportI1 T_Type T_IsAssignableFrom T_Type where
  methodDynImportI1 = makeTypeIsAssignableFrom

--
-- System.Type.GetType(System.String)
--
typeGetType :: T.Text -> IO (Object T_Type)
typeGetType = invokeS @T_GetType @T_Type

--
-- System.Type.IsAssignableFrom(System.Type)
--
typeIsAssignableFrom :: Object T_Type -> Object T_Type -> IO (Bool)
typeIsAssignableFrom t1 t2 = invokeI @T_IsAssignableFrom t1 t2

--
-- Marshaling Object <-> GCHandle
--

-- clr-typed doesn't know of GCHandle as it is provided by clr-host
type instance BridgeTypeObject t = GCHandle t

-- Calling a CLR function from Haskell
instance {-# OVERLAPS #-} Marshal (Object t) (GCHandle t) where
  marshal (Object x) f = withForeignPtr x $ \x'-> f $ coerce x'

-- Calling a Haskell function from the CLR
instance {-# OVERLAPS #-} (TString t) => Marshal (GCHandle t) (Object t) where
  marshal x f = do
    x' <- newHandle x
    finalizer <- gcHandleFinalizer
    fp <- newForeignPtr finalizer (coerce x')
    f $ Object fp

-- Returning from a CLR function that was called from Haskell
instance {-# OVERLAPPING #-} (TString t) => Unmarshal (GCHandle t) (Object t) where
  unmarshal x = do
    finalizer <- gcHandleFinalizer
    fp <- newForeignPtr finalizer (coerce x)
    return $ Object fp

-- Returning from a Haskell function that was called by the CLR
instance {-# OVERLAPPING #-} (TString t) => Unmarshal (Object t) (GCHandle t) where
  unmarshal (Object x) = withForeignPtr x $ \x'-> newHandle $ coerce x'



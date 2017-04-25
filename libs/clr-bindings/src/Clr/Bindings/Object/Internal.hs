{-# LANGUAGE TypeInType, TypeApplications, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances, TypeFamilies #-}

module Clr.Bindings.Object.Internal
  ( typeGetType
  , typeIsAssignableFrom
  , T_Type
  , T_ToString
  , T_GetType
  ) where

import Clr
import Clr.TypeString

import Clr.Host
import Clr.Host.BStr

import Clr.Marshal

import Clr.Bindings.DynImports
import Clr.Bindings.BStr

import Data.Text as T

import Foreign.Ptr

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
type T_FullName            = T "FullName" '[]
type T_GetGenericArguments = T "GetGenericArguments" '[]
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
type instance Members T_Type   = '[ T_FullName
                                  , T_GetGenericArguments
                                  , T_GetMembers
                                  , T_GetType
                                  , T_IsAssignableFrom
                                  , T_Namespace              ]

type instance Candidates T_object T_ToString          = '[ '[ ] ]
type instance Candidates T_object T_GetType           = '[ '[ ] ]
type instance Candidates T_Type   T_GetType           = '[ '[ T_string ] ]
type instance Candidates T_Type   T_IsAssignableFrom  = '[ '[ T_Type ] ]

foreign import ccall "dynamic" makeObjToString :: FunPtr (ObjectID t -> IO BStr) -> (ObjectID t -> IO BStr)
foreign import ccall "dynamic" makeObjGetType  :: FunPtr (ObjectID t -> IO (ObjectID (T "System.Type" '[]))) -> (ObjectID t -> IO (ObjectID (T "System.Type" '[])))
foreign import ccall "dynamic" makeTypeGetType :: FunPtr (BStr -> IO (ObjectID T_Type)) -> (BStr -> IO (ObjectID T_Type))
foreign import ccall "dynamic" makeTypeIsAssignableFrom      :: FunPtr (ObjectID T_Type -> ObjectID T_Type -> IO Bool) -> (ObjectID T_Type -> ObjectID T_Type -> IO Bool)

instance MethodResultI1 T_object T_ToString () where
  type ResultTypeI1 T_object T_ToString () = 'Just T_string

instance MethodResultI1 T_object T_GetType () where
  type ResultTypeI1 T_object T_GetType () = 'Just T_Type

instance MethodResultS1 T_Type T_GetType T_string where
  type ResultTypeS1 T_Type T_GetType T_string = 'Just T_Type

instance MethodResultI1 T_Type T_IsAssignableFrom T_Type where
  type ResultTypeI1 T_Type T_IsAssignableFrom T_Type = 'Just T_bool

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

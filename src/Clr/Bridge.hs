{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, TypeFamilies, UndecidableInstances, TypeInType #-}

module Clr.Bridge where

import Clr.ListTuple
import Clr.Object
import Clr.Types

import Data.Kind
import Data.Type.Bool

import Data.Int
import Data.Word
import Foreign.C
import Foreign.Ptr

--
-- Bridge type goes from something like "System.String" to CString
--
type family BridgeType (x::Type) :: Type where
  BridgeType () = ()
  BridgeType a  = If (IsPrimType a) (BridgeTypePrim a) ObjectID

--
-- Maybe on bridge types, choosing () for Nothing
--
type family BridgeTypeM (x::Maybe Type) :: Type where
  BridgeTypeM 'Nothing = ()
  BridgeTypeM ('Just x) = BridgeType x

--
-- Bridge types of each primitive
--
type family BridgeTypePrim (x::Type)

type instance BridgeTypePrim (T "System.String" 'Nothing '[])  = CString
type instance BridgeTypePrim (T "System.Int16" 'Nothing '[])   = Int16
type instance BridgeTypePrim (T "System.UInt16" 'Nothing '[])  = Word16
type instance BridgeTypePrim (T "System.Int32" 'Nothing '[])   = Int32
type instance BridgeTypePrim (T "System.UInt32" 'Nothing '[])  = Word32
type instance BridgeTypePrim (T "System.Int64" 'Nothing '[])   = Int64
type instance BridgeTypePrim (T "System.UInt64" 'Nothing '[])  = Word64
type instance BridgeTypePrim (T "System.IntPtr" 'Nothing '[])  = IntPtr
type instance BridgeTypePrim (T "System.UIntPtr" 'Nothing '[]) = WordPtr
type instance BridgeTypePrim (T "System.Char" 'Nothing '[])    = Char
type instance BridgeTypePrim (T "System.Single" 'Nothing '[])  = CFloat
type instance BridgeTypePrim (T "System.Double" 'Nothing '[])  = CDouble

--
-- Bridge type that operates on lists
--
type family BridgeTypeL (a::[Type]) :: [Type] where
  BridgeTypeL '[] = '[]
  BridgeTypeL (x ': xs) = BridgeType x ': BridgeTypeL xs

--
-- Bridge types with a param of a list and the result as a tuple
--
type family BridgeTypes (x::[Type]) :: Type where
  BridgeTypes x = ListToTuple (BridgeTypeL x)



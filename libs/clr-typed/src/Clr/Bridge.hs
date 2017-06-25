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
-- BridgeType goes from the Haskell representation of a CLR type to the
-- low level type that it is marshaled as to cross the bridge
--
type family BridgeType (x::Type) :: Type where
  BridgeType () = ()
  BridgeType t  = If (IsPrimType t) (BridgeTypePrim t) (BridgeTypeObject t)

--
-- Bridge types of each primitive
--
type family BridgeTypePrim (x::Type)

type instance BridgeTypePrim (T "System.SByte"   '[]) = Int8
type instance BridgeTypePrim (T "System.Byte"    '[]) = Word8
type instance BridgeTypePrim (T "System.Int16"   '[]) = Int16
type instance BridgeTypePrim (T "System.UInt16"  '[]) = Word16
type instance BridgeTypePrim (T "System.Int32"   '[]) = Int32
type instance BridgeTypePrim (T "System.UInt32"  '[]) = Word32
type instance BridgeTypePrim (T "System.Int64"   '[]) = Int64
type instance BridgeTypePrim (T "System.UInt64"  '[]) = Word64
type instance BridgeTypePrim (T "System.IntPtr"  '[]) = IntPtr
type instance BridgeTypePrim (T "System.UIntPtr" '[]) = WordPtr
type instance BridgeTypePrim (T "System.Char"    '[]) = Char
type instance BridgeTypePrim (T "System.Single"  '[]) = CFloat
type instance BridgeTypePrim (T "System.Double"  '[]) = CDouble
type instance BridgeTypePrim (T "System.Boolean" '[]) = Bool
type instance BridgeTypePrim (T "System.Void"    '[]) = ()


--
-- Bridge type of Object is left uninstantiated in this package
--

type family BridgeTypeObject (x::Type)

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


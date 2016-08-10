{-# LANGUAGE PolyKinds, DataKinds, TypeOperators, TypeFamilies, UndecidableInstances #-}

module Clr.Bridge where

import Clr.Common

import Data.Kind
import Data.Type.Bool
import GHC.TypeLits

import Data.Int
import Data.Word
import Foreign.C
import Foreign.Ptr

type family BridgeTypeM (x::Maybe Symbol) :: Type where
  BridgeTypeM 'Nothing = ()
  BridgeTypeM ('Just x) = BridgeType x

type family BridgeType (x::Symbol) :: Type where
  BridgeType a = If (IsPrimType a) (BridgeTypePrim a) ObjectID

type family BridgeTypePrim (x::Symbol)

type instance BridgeTypePrim "System.String"  = CString
type instance BridgeTypePrim "System.Int16"   = Int16
type instance BridgeTypePrim "System.UInt16"  = Word16
type instance BridgeTypePrim "System.Int32"   = Int32
type instance BridgeTypePrim "System.UInt32"  = Word32
type instance BridgeTypePrim "System.Int64"   = Int64
type instance BridgeTypePrim "System.UInt64"  = Word64
type instance BridgeTypePrim "System.IntPtr"  = IntPtr
type instance BridgeTypePrim "System.UIntPtr" = WordPtr
type instance BridgeTypePrim "System.Char"    = Char
type instance BridgeTypePrim "System.Single"  = CFloat
type instance BridgeTypePrim "System.Double"  = CDouble

type family BridgeTypeL (a::[Symbol]) :: [Type] where
  BridgeTypeL '[] = '[]
  BridgeTypeL (x ': xs) = BridgeType x ': BridgeTypeL xs

type family BridgeTypes (x::[Symbol]) :: Type where
  BridgeTypes x = ListToTuple (BridgeTypeL x)


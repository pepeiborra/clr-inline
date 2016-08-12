{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr (invokeS, MethodS(..), invokeI, MethodI(..), Members, SuperTypeOf, ObjectID, Object(..) ) where

import Clr.Common
import Clr.Bridge
import Clr.Prim
import Clr.ListTuple
import Clr.Curry
import Clr.Marshal

import Foreign.C
import Data.Int
import Data.Word
import Data.Bool
import Foreign.Ptr
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Data.Kind

import Unsafe.Coerce

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

type instance SuperTypeOf "System.Object"    = 'Nothing
type instance SuperTypeOf "System.ValueType" = 'Just "System.Object"

type instance SuperTypeOf "System.String"  = 'Just "System.Object"
type instance SuperTypeOf "System.Int16"   = 'Just "System.ValueType"
type instance SuperTypeOf "System.UInt16"  = 'Just "System.ValueType"
type instance SuperTypeOf "System.Int32"   = 'Just "System.ValueType"
type instance SuperTypeOf "System.UInt32"  = 'Just "System.ValueType"
type instance SuperTypeOf "System.Int64"   = 'Just "System.ValueType"
type instance SuperTypeOf "System.UInt64"  = 'Just "System.ValueType"
type instance SuperTypeOf "System.IntPtr"  = 'Just "System.ValueType"
type instance SuperTypeOf "System.UIntPtr" = 'Just "System.ValueType"
type instance SuperTypeOf "System.Char"    = 'Just "System.ValueType"
type instance SuperTypeOf "System.Single"  = 'Just "System.ValueType"
type instance SuperTypeOf "System.Double"  = 'Just "System.ValueType"


type ObjectID = Int64

data Object (typ::Symbol) = Object ObjectID

type family SuperTypeOf (t::Symbol) :: Maybe Symbol

type family SuperTypesOf' (t :: Maybe Symbol) :: [Maybe Symbol] where
  SuperTypesOf' ('Just x) = (SuperTypeOf x) ': (SuperTypesOf' (SuperTypeOf x))
  SuperTypesOf' 'Nothing  = '[]

type family SuperTypesOf (t :: Symbol) :: [Symbol] where
  SuperTypesOf x = UnMaybeList (SuperTypesOf' ('Just x))

type family InheritsFrom (t1 :: Symbol) (t2 :: Symbol) :: Bool where
  InheritsFrom t1 t2 = t1 == t2 || Elem t2 (SuperTypesOf t1)

type family IsValueType (a::Symbol) :: Bool where
  IsValueType a = a `InheritsFrom` "System.ValueType"

type family IsPrimType (a::Symbol) :: Bool where
  IsPrimType "System.String" = True
  IsPrimType  a              = IsValueType a

type family IsRefType (a::Symbol) :: Bool where
  IsRefType a = Not (IsValueType a)


class MethodS (t::Symbol) (m::Symbol) (args::[Symbol]) where
  type ResultTypeS t m args :: Maybe Symbol
  rawInvokeS :: CurryT (BridgeTypes args) (IO (BridgeTypeM (ResultTypeS t m args)))

class MethodI (t::Symbol) (m::Symbol) (args::[Symbol]) where
  type ResultTypeI t m args :: Maybe Symbol
  rawInvokeI :: (BridgeType t) -> CurryT (BridgeTypes args) (IO (BridgeTypeM (ResultTypeI t m args)))

type family HasMember (t::Symbol) (m::Symbol) :: Bool where
  HasMember t m = m `Elem` (Members t)

type family UnBridgeType (t::Type) :: [Symbol] where
  UnBridgeType String = '["System.String"]
  UnBridgeType Int32 = '["System.Int32"]
  UnBridgeType Int64 = '["System.Int64"]
  UnBridgeType (a, b) = UnBridgeType a `Concat` UnBridgeType b

type family ResolveArgTypes t m (args'::Type) :: [Symbol] where
  ResolveArgTypes t m args' = UnBridgeType args'

type family Members (t::Symbol) :: [Symbol]

type family ResolveBaseType (t::Symbol) (m::Symbol) :: Symbol where
  ResolveBaseType t m = ResolveBaseType' ('Just t) m

type family ResolveBaseType' (t::Maybe Symbol) (m::Symbol) :: Symbol where
  ResolveBaseType' 'Nothing  m = "Error"
  ResolveBaseType' ('Just t) m = If (m `Elem` (Members t)) t (ResolveBaseType' (SuperTypeOf t) m)

invokeS :: forall m t args args'. ( ResolveArgTypes t m args' ~ args
                                  , MethodS t m args
                                  , Marshal args' (BridgeTypes args)
                                  , Unmarshal (BridgeTypeM (ResultTypeS t m args)) (UnmarshalAs (BridgeTypeM (ResultTypeS t m args)))
                                  , Curry' (TupleSize args') ((BridgeTypes args) -> (IO (BridgeTypeM (ResultTypeS t m args)))) (CurryT (BridgeTypes args) (IO (BridgeTypeM (ResultTypeS t m args))))
                                  ) => args' -> IO (UnmarshalAs (BridgeTypeM (ResultTypeS t m args)))
invokeS x = marshal @args' @(BridgeTypes args) @((BridgeTypeM (ResultTypeS t m args))) x (\tup-> uncurryN' @(TupleSize args') (rawInvokeS @t @m @args) tup) >>= unmarshal

invokeI :: forall m t t' args args'. ( ResolveBaseType t' m ~ t
                                     , ResolveArgTypes t m args' ~ args
                                     , MethodI t m args
                                     , Marshal args' (BridgeTypes args)
                                     , Marshal (Object t) (BridgeType t)
                                     , Unmarshal (BridgeTypeM (ResultTypeI t m args)) (UnmarshalAs (BridgeTypeM (ResultTypeI t m args)))
                                     , Curry' (TupleSize args') ((BridgeTypes args) -> (IO (BridgeTypeM (ResultTypeI t m args)))) (CurryT (BridgeTypes args) (IO (BridgeTypeM (ResultTypeI t m args))))
                                     ) => Object t' -> args' -> IO (UnmarshalAs (BridgeTypeM (ResultTypeI t m args)))
invokeI obj x = marshal @args' @(BridgeTypes args) @((BridgeTypeM (ResultTypeI t m args))) x (\tup-> marshal @(Object t) @(BridgeType t) @((BridgeTypeM (ResultTypeI t m args))) (upcast obj) (\obj'-> uncurryN' @(TupleSize args') (rawInvokeI @t @m @args obj') tup)) >>= unmarshal

upcast :: Object t -> Object t'
upcast = unsafeCoerce


class Marshal a b where
  marshal :: a -> (b-> IO c) -> IO c

instance Marshal String CString where
  marshal = withCString

instance Marshal (Object t) ObjectID where
  marshal (Object x) f = f x

instance Marshal Int32 Int32 where
  marshal x f = f x

instance Marshal Int64 Int64 where
  marshal x f = f x

instance (Marshal a1 b1, Marshal a2 b2) => Marshal (a1, a2) (b1, b2) where
  marshal (x1,x2) f = marshal x1 (\x1'-> marshal x2 (\x2'-> f (x1', x2')))

type family UnmarshalAs (x::k) :: k'

type instance UnmarshalAs () = ()
type instance UnmarshalAs CString   = String

class Unmarshal a b where
  unmarshal :: a -> IO b

instance Unmarshal CString String where
  unmarshal cs = do
    s <- peekCString cs
    -- free cs
    return s

instance Unmarshal () () where
  unmarshal = return





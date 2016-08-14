{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr (invokeS, MethodS(..), invokeI, MethodI(..), Members, SuperTypeOf, ObjectID, Object(..) ) where

import Clr.Bridge
import Clr.Curry
import Clr.Inheritance
import Clr.ListTuple
import Clr.Marshal
import Clr.Object

import Foreign.C
import Data.Int
import Data.Word
import Data.Bool
import Foreign.Ptr
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Data.Kind



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
  ResolveBaseType' ('Just t) m = If (t `HasMember` m) t (ResolveBaseType' (SuperTypeOf t) m)

invokeS :: forall m t args args'. ( ResolveArgTypes t m args' ~ args
                                  , MethodS t m args
                                  , Marshal args' (BridgeTypes args)
                                  , Unmarshal (BridgeTypeM (ResultTypeS t m args)) (UnmarshalAs (BridgeTypeM (ResultTypeS t m args)))
                                  , Curry' (TupleSize args') ((BridgeTypes args) -> (IO (BridgeTypeM (ResultTypeS t m args)))) (CurryT (BridgeTypes args) (IO (BridgeTypeM (ResultTypeS t m args))))
                                  ) => args' -> IO (UnmarshalAs (BridgeTypeM (ResultTypeS t m args)))
invokeS x = marshal @args' @(BridgeTypes args) @((BridgeTypeM (ResultTypeS t m args))) x (\tup-> uncurryN' @(TupleSize args') (rawInvokeS @t @m @args) tup) >>= unmarshal

invokeI :: forall m t t' args args'. ( ResolveBaseType t' m ~ t
                                     , t' `InheritsFrom` t ~ True
                                     , ResolveArgTypes t m args' ~ args
                                     , MethodI t m args
                                     , Marshal args' (BridgeTypes args)
                                     , Marshal (Object t) (BridgeType t)
                                     , Unmarshal (BridgeTypeM (ResultTypeI t m args)) (UnmarshalAs (BridgeTypeM (ResultTypeI t m args)))
                                     , Curry' (TupleSize args') ((BridgeTypes args) -> (IO (BridgeTypeM (ResultTypeI t m args)))) (CurryT (BridgeTypes args) (IO (BridgeTypeM (ResultTypeI t m args))))
                                     ) => Object t' -> args' -> IO (UnmarshalAs (BridgeTypeM (ResultTypeI t m args)))
invokeI obj x = marshal @args' @(BridgeTypes args) @((BridgeTypeM (ResultTypeI t m args))) x (\tup-> marshal @(Object t) @(BridgeType t) @((BridgeTypeM (ResultTypeI t m args))) (upCast obj) (\obj'-> uncurryN' @(TupleSize args') (rawInvokeI @t @m @args obj') tup)) >>= unmarshal



--
-- Todo: Move more code to seperate modules when GHC bug is fixed
--

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



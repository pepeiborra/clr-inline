{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr (invokeS, MethodS(..), invokeI, MethodI(..), new, Constructor(..), Members, SuperTypeOf, ObjectID, Object(..) ) where

import Clr.Bridge
import Clr.Curry
import Clr.Inheritance
import Clr.ListTuple
import Clr.Marshal
import Clr.Object

import Foreign.C
import Data.Int
import Data.Word
import Foreign.Ptr
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Data.Kind


--
-- Static method
--
class MethodS (t::Symbol) (m::Symbol) (args::[Symbol]) where
  type ResultTypeS t m args :: Maybe Symbol
  rawInvokeS :: CurryT (BridgeTypes args) (IO (BridgeTypeM (ResultTypeS t m args)))

--
-- Instance method
--
class MethodI (t::Symbol) (m::Symbol) (args::[Symbol]) where
  type ResultTypeI t m args :: Maybe Symbol
  rawInvokeI :: (BridgeType t) -> CurryT (BridgeTypes args) (IO (BridgeTypeM (ResultTypeI t m args)))

--
-- Constructor
--
class Constructor (t::Symbol) (args::[Symbol]) where
  rawNew :: CurryT (BridgeTypes args) (IO (BridgeType t))

--
-- Declaration of all compile type chooseable members of a particular type
--
type family Members (t::Symbol) :: [Symbol]

--
-- t `HasMember` m ~ True t has declared m within its members
--
type family HasMember (t::Symbol) (m::Symbol) :: Bool where
  HasMember t m = m `Elem` (Members t)

--
-- Overload resolution. t is something like "Console". m is something like "WriteLine".
-- args' is Something like String. This should then result in something like "System.String".
--
type family ResolveArgTypes t m (args'::Type) :: [Symbol] where
  ResolveArgTypes t m args' = UnBridgeType args'

--
-- Something simple just to get the above working for now
--
type family UnBridgeType (t::Type) :: [Symbol] where
  UnBridgeType String = '["System.String"]
  UnBridgeType Int32  = '["System.Int32"]
  UnBridgeType Int64  = '["System.Int64"]
  UnBridgeType ()     = '[]
  UnBridgeType (a, b) = UnBridgeType a `Concat` UnBridgeType b

--
-- When a method m is invoked on a type t, we need to go up the hierarchy
-- to find the type that t derives from that declared m
--
type family ResolveBaseType (t::Symbol) (m::Symbol) :: Symbol where
  ResolveBaseType t m = ResolveBaseType' ('Just t) m

type family ResolveBaseType' (t::Maybe Symbol) (m::Symbol) :: Symbol where
  ResolveBaseType' 'Nothing  m = "Error"
  ResolveBaseType' ('Just t) m = If (t `HasMember` m) t (ResolveBaseType' (SuperTypeOf t) m)

--
-- Static method invocation
--
invokeS :: forall m t args args'. ( ResolveArgTypes t m args' ~ args
                                  , MethodS t m args
                                  , Marshal args' (BridgeTypes args)
                                  , Unmarshal (BridgeTypeM (ResultTypeS t m args)) (UnmarshalAs (BridgeTypeM (ResultTypeS t m args)))
                                  , Curry (TupleSize args') ((BridgeTypes args) -> (IO (BridgeTypeM (ResultTypeS t m args)))) (CurryT (BridgeTypes args) (IO (BridgeTypeM (ResultTypeS t m args))))
                                  ) => args' -> IO (UnmarshalAs (BridgeTypeM (ResultTypeS t m args)))
invokeS x = marshal @args' @(BridgeTypes args) @((BridgeTypeM (ResultTypeS t m args))) x (\tup-> uncurryN @(TupleSize args') (rawInvokeS @t @m @args) tup) >>= unmarshal

--
-- Instance method invocation
--
invokeI :: forall m t t' args args'. ( ResolveBaseType t' m ~ t
                                     , t' `InheritsFrom` t ~ 'True
                                     , ResolveArgTypes t m args' ~ args
                                     , MethodI t m args
                                     , Marshal args' (BridgeTypes args)
                                     , Marshal (Object t) (BridgeType t)
                                     , Unmarshal (BridgeTypeM (ResultTypeI t m args)) (UnmarshalAs (BridgeTypeM (ResultTypeI t m args)))
                                     , Curry (TupleSize args') ((BridgeTypes args) -> (IO (BridgeTypeM (ResultTypeI t m args)))) (CurryT (BridgeTypes args) (IO (BridgeTypeM (ResultTypeI t m args))))
                                     ) => Object t' -> args' -> IO (UnmarshalAs (BridgeTypeM (ResultTypeI t m args)))
invokeI obj x = marshal @args' @(BridgeTypes args) @((BridgeTypeM (ResultTypeI t m args))) x (\tup-> marshal @(Object t) @(BridgeType t) @((BridgeTypeM (ResultTypeI t m args))) (upCast obj) (\obj'-> uncurryN @(TupleSize args') (rawInvokeI @t @m @args obj') tup)) >>= unmarshal

--
-- Constructor invocation
--
new :: forall t args args' . ( ResolveArgTypes t t args' ~ args
                             , Constructor t args
                             , Marshal args' (BridgeTypes args)
                             , Unmarshal (BridgeType t) (Object t)
                             , Curry (TupleSize args') ((BridgeTypes args) -> (IO (BridgeType t))) (CurryT (BridgeTypes args) (IO (BridgeType t)))
                             ) => args' -> IO (Object t)
new x = marshal @args' @(BridgeTypes args) @(BridgeType t) x (\tup-> uncurryN @(TupleSize args') (rawNew @t @args) tup) >>= unmarshal



--
-- I attempted to move the following lines into a sperate module Clr.Bridge but it
-- wouldn't copmpile with stack build. Perhaps some obscure ghc bug since
-- it would load fine stack ghci though. Could do with further investigation.
--

--
-- Bridge type goes from something like "System.String" to CString
--
type family BridgeType (x::Symbol) :: Type where
  BridgeType a = If (IsPrimType a) (BridgeTypePrim a) ObjectID

--
-- Maybe on bridge types, choosing () for Nothing
--
type family BridgeTypeM (x::Maybe Symbol) :: Type where
  BridgeTypeM 'Nothing = ()
  BridgeTypeM ('Just x) = BridgeType x

--
-- Bridge types of each primitive
--
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

--
-- Bridge type that operates on lists
--
type family BridgeTypeL (a::[Symbol]) :: [Type] where
  BridgeTypeL '[] = '[]
  BridgeTypeL (x ': xs) = BridgeType x ': BridgeTypeL xs

--
-- Bridge types with a param of a list a result as a tuple
--
type family BridgeTypes (x::[Symbol]) :: Type where
  BridgeTypes x = ListToTuple (BridgeTypeL x)



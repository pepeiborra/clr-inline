{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr (invokeS, MethodS1(..), MethodS2(..), invokeI, MethodI1(..), MethodI2(..), new, Constructor1(..), Constructor2(..), Members, SuperTypeOf, ObjectID, T, Object(..), BridgeType, BridgeTypeM, BridgeTypes, CurryT, GenT ) where

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
import Data.Kind


--
-- Static methods
--

class MethodS1 (t::Type) (m::Type) (arg0::Type) where
  type ResultTypeS1 t m arg0 :: Maybe Type
  rawInvokeS1 :: (BridgeType arg0) -> (IO (BridgeTypeM (ResultTypeS1 t m arg0)))

class MethodS2 (t::Type) (m::Type) (arg0::Type) (arg1::Type) where
  type ResultTypeS2 t m arg0 arg1 :: Maybe Type
  rawInvokeS2 :: (BridgeType arg0) -> (BridgeType arg1) -> (IO (BridgeTypeM (ResultTypeS2 t m arg0 arg1)))

--
-- Unification of static methods
--

class MethodS (n::Nat) (t::Type) (m::Type) (args::[Type]) where
  type ResultTypeS n t m args :: Maybe Type
  rawInvokeS :: CurryT' n (BridgeTypes args) (IO (BridgeTypeM (ResultTypeS n t m args)))

instance (MethodS1 t m ()) => MethodS 1 t m '[] where
  type ResultTypeS 1 t m '[] = ResultTypeS1 t m ()
  rawInvokeS = rawInvokeS1 @t @m @()

instance (MethodS1 t m a) => MethodS 1 t m '[a] where
  type ResultTypeS 1 t m '[a] = ResultTypeS1 t m a
  rawInvokeS = rawInvokeS1 @t @m @a

instance (MethodS2 t m a0 a1) => MethodS 2 t m '[a0, a1] where
  type ResultTypeS 2 t m '[a0, a1] = ResultTypeS2 t m a0 a1
  rawInvokeS = rawInvokeS2 @t @m @a0 @a1

--
-- Instance methods
--

class MethodI1 (t::Type) (m::Type) (arg0::Type) where
  type ResultTypeI1 t m arg0 :: Maybe Type
  rawInvokeI1 :: (BridgeType t) -> (BridgeType arg0) -> (IO (BridgeTypeM (ResultTypeI1 t m arg0)))

class MethodI2 (t::Type) (m::Type) (arg0::Type) (arg1::Type) where
  type ResultTypeI2 t m arg0 arg1 :: Maybe Type
  rawInvokeI2 :: (BridgeType t) -> (BridgeType arg0) -> (BridgeType arg1) -> (IO (BridgeTypeM (ResultTypeI2 t m arg0 arg1)))

--
-- Unification of instance methods
--

class MethodI (n::Nat) (t::Type) (m::Type) (args::[Type]) where
  type ResultTypeI n t m args :: Maybe Type
  rawInvokeI :: (BridgeType t) -> CurryT' n (BridgeTypes args) (IO (BridgeTypeM (ResultTypeI n t m args)))

instance (MethodI1 t m ()) => MethodI 1 t m '[] where
  type ResultTypeI 1 t m '[] = ResultTypeI1 t m ()
  rawInvokeI = rawInvokeI1 @t @m @()

instance (MethodI1 t m a) => MethodI 1 t m '[a] where
  type ResultTypeI 1 t m '[a] = ResultTypeI1 t m a
  rawInvokeI = rawInvokeI1 @t @m @a

instance (MethodI2 t m a0 a1) => MethodI 2 t m '[a0, a1] where
  type ResultTypeI 2 t m '[a0, a1] = ResultTypeI2 t m a0 a1
  rawInvokeI = rawInvokeI2 @t @m @a0 @a1

--
-- Constructors
--
class Constructor1 (t::Type) (arg0::Type) where
  rawNew1 :: (BridgeType arg0) -> (IO (BridgeType t))

class Constructor2 (t::Type) (arg0::Type) (arg1::Type) where
  rawNew2 :: (BridgeType arg0) -> (BridgeType arg1) -> (IO (BridgeType t))

--
-- Unification of constructors
--

class Constructor (n::Nat) (t::Type) (args::[Type]) where
  rawNew :: CurryT' n (BridgeTypes args) (IO (BridgeType t))

instance (Constructor1 t ()) => Constructor 1 t '[] where
  rawNew = rawNew1 @t @()

instance (Constructor1 t a) => Constructor 1 t '[a] where
  rawNew = rawNew1 @t @a

instance (Constructor2 t a0 a1) => Constructor 2 t '[a0, a1] where
  rawNew = rawNew2 @t @a0 @a1

--
-- Declaration of all compile type chooseable members of a particular type
--
type family Members (t::Type) :: [Type]

--
-- t `HasMember` m ~ True t has declared m within its members
--
type family HasMember (t::Type) (m::Type) :: Bool where
  HasMember t m = m `Elem` (Members t)

--
-- Overload resolution. t is something like "Console". m is something like "WriteLine".
-- args' is Something like String. This should then result in something like "System.String".
--
type family ResolveArgTypes t m (args'::Type) :: [Type] where
  ResolveArgTypes t m args' = UnBridgeType args'

--
-- Something simple just to get the above working for now
--
type family UnBridgeType (t::Type) :: [Type] where
  UnBridgeType String = '[(T "System.String" '[])]
  UnBridgeType Int32  = '[(T "System.Int32" '[])]
  UnBridgeType Int64  = '[(T "System.Int64" '[])]
  UnBridgeType ()     = '[]
  UnBridgeType (a, b) = UnBridgeType a `Concat` UnBridgeType b

--
-- When a method m is invoked on a type t, we need to go up the hierarchy
-- to find the type that t derives from that declared m
--
type family ResolveBaseType (t::Type) (m::Type) :: Type where
  ResolveBaseType t m = ResolveBaseType' ('Just t) m

type family ResolveBaseType' (t::Maybe Type) (m::Type) :: Type where
  ResolveBaseType' 'Nothing  m = Error "No Base Type Of Nothing"
  ResolveBaseType' ('Just t) m = If (t `HasMember` m) t (ResolveBaseType' (SuperTypeOf t) m)

data Error (s::Symbol) = Error

--
-- Static method invocation
--
invokeS :: forall ms ts m t args args' n. ( ToClrType ms ~ m
                                          , ToClrType ts ~ t
                                          , TupleSize args' ~ n
                                          , ResolveArgTypes t m args' ~ args
                                          , MethodS n t m args
                                          , Marshal args' (BridgeTypes args)
                                          , Unmarshal (BridgeTypeM (ResultTypeS n t m args)) (UnmarshalAs (BridgeTypeM (ResultTypeS n t m args)))
                                          , Curry n ((BridgeTypes args) -> (IO (BridgeTypeM (ResultTypeS n t m args)))) (CurryT' n (BridgeTypes args) (IO (BridgeTypeM (ResultTypeS n t m args))))
                                          ) => args' -> IO (UnmarshalAs (BridgeTypeM (ResultTypeS n t m args)))
invokeS x = marshal @args' @(BridgeTypes args) @((BridgeTypeM (ResultTypeS n t m args))) x (\tup-> uncurryN @n (rawInvokeS @n @t @m @args) tup) >>= unmarshal

--
-- Instance method invocation
--

invokeI :: forall ms m t t' args args' n. ( ToClrType ms ~ m
                                          , TupleSize args' ~ n
                                          , ResolveBaseType t' m ~ t
                                          , t' `InheritsFrom` t ~ 'True
                                          , ResolveArgTypes t m args' ~ args
                                          , MethodI n t m args
                                          , Marshal args' (BridgeTypes args)
                                          , Marshal (Object t) (BridgeType t)
                                          , Unmarshal (BridgeTypeM (ResultTypeI n t m args)) (UnmarshalAs (BridgeTypeM (ResultTypeI n t m args)))
                                          , Curry n ((BridgeTypes args) -> (IO (BridgeTypeM (ResultTypeI n t m args)))) (CurryT' n (BridgeTypes args) (IO (BridgeTypeM (ResultTypeI n t m args))))
                                          ) => Object t' -> args' -> IO (UnmarshalAs (BridgeTypeM (ResultTypeI n t m args)))
invokeI obj x = marshal @args' @(BridgeTypes args) @((BridgeTypeM (ResultTypeI n t m args))) x (\tup-> marshal @(Object t) @(BridgeType t) @((BridgeTypeM (ResultTypeI n t m args))) (upCast obj) (\obj'-> uncurryN @n (rawInvokeI @n @t @m @args obj') tup)) >>= unmarshal




--
-- Constructor invocation
--
new :: forall ts t args args' n. ( ToClrType ts ~ t
                                 , TupleSize args' ~ n
                                 , ResolveArgTypes t t args' ~ args
                                 , Constructor n t args
                                 , Marshal args' (BridgeTypes args)
                                 , Unmarshal (BridgeType t) (Object t)
                                 , Curry n ((BridgeTypes args) -> (IO (BridgeType t))) (CurryT' n (BridgeTypes args) (IO (BridgeType t)))
                                 ) => args' -> IO (Object t)
new x = marshal @args' @(BridgeTypes args) @(BridgeType t) x (\tup-> uncurryN @n (rawNew @n @t @args) tup) >>= unmarshal



--
-- I attempted to move the following lines into a sperate module Clr.Bridge but it
-- wouldn't copmpile with stack build. Perhaps some obscure ghc bug since
-- it would load fine stack ghci though. Could do with further investigation.
--

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

type instance BridgeTypePrim (T "System.String" '[])  = CString
type instance BridgeTypePrim (T "System.Int16" '[])   = Int16
type instance BridgeTypePrim (T "System.UInt16" '[])  = Word16
type instance BridgeTypePrim (T "System.Int32" '[])   = Int32
type instance BridgeTypePrim (T "System.UInt32" '[])  = Word32
type instance BridgeTypePrim (T "System.Int64" '[])   = Int64
type instance BridgeTypePrim (T "System.UInt64" '[])  = Word64
type instance BridgeTypePrim (T "System.IntPtr" '[])  = IntPtr
type instance BridgeTypePrim (T "System.UIntPtr" '[]) = WordPtr
type instance BridgeTypePrim (T "System.Char" '[])    = Char
type instance BridgeTypePrim (T "System.Single" '[])  = CFloat
type instance BridgeTypePrim (T "System.Double" '[])  = CDouble

--
-- Bridge type that operates on lists
--
type family BridgeTypeL (a::[Type]) :: [Type] where
  BridgeTypeL '[] = '[]
  BridgeTypeL (x ': xs) = BridgeType x ': BridgeTypeL xs

--
-- Bridge types with a param of a list a result as a tuple
--
type family BridgeTypes (x::[Type]) :: Type where
  BridgeTypes x = ListToTuple (BridgeTypeL x)



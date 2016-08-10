{-# LANGUAGE GADTs, DataKinds, KindSignatures, PolyKinds, TypeFamilies, TypeOperators, RankNTypes, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, ScopedTypeVariables, ExistentialQuantification #-}
{-# LANGUAGE UndecidableInstances, TypeApplications, AllowAmbiguousTypes, TypeInType, TypeFamilyDependencies, FunctionalDependencies #-}

module Clr (invokeS, MethodS(..), invokeI, MethodI(..), module Clr.Prim, module Clr.Common ) where

import Clr.Resolver
import Clr.Common
import Clr.Bridge
import Clr.Prim

import Foreign.C
import Data.Int
import Data.Word
import Data.Bool
import Foreign.Ptr
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality
import Data.Kind

import Data.Tuple.Curry


type family TupleSize (x::k) :: Nat where
  TupleSize (a,b,c,d) = 4
  TupleSize (a,b,c) = 3
  TupleSize (a,b) = 2
  TupleSize (a) = 1

type family ListSize (x::[k]) :: Nat where
  ListSize '[a, b, c, d, e, f] = 6
  ListSize '[a, b, c, d, e] = 5
  ListSize '[a, b, c, d] = 4
  ListSize '[a, b, c] = 3
  ListSize '[a, b] = 2
  ListSize '[a] = 1

type family CurryT x r where
  CurryT (a,b,c,d) r = a -> b -> c -> d -> r
  CurryT (a,b,c) r = a -> b -> c -> r
  CurryT (a,b) r = a -> b -> r
  CurryT a r = a -> r

class Curry' (n::Nat) a b | n b -> a, n a -> b where
  curryN' :: a -> b
  uncurryN' :: b -> a

instance Curry' 1 (a -> r) (a -> r) where
  curryN' = id
  uncurryN' = id

instance Curry' 2 ((a,b) -> r) (a -> b -> r)  where
  curryN' = curryN
  uncurryN' = uncurryN

instance Curry' 3 ((a,b,c) -> r) (a -> b -> c -> r)  where
  curryN' = curryN
  uncurryN' = uncurryN

instance Curry' 4 ((a,b,c,d) -> r) (a -> b -> c -> d -> r)  where
  curryN' = curryN
  uncurryN' = uncurryN


class MethodS (t::Symbol) (m::Symbol) (args::[Symbol]) where
  type ResultTypeS t m args :: Maybe Symbol
  rawInvokeS :: CurryT (BridgeTypes args) (IO (BridgeTypeM (ResultTypeS t m args)))

class MethodI (t::Symbol) (m::Symbol) (args::[Symbol]) where
  type ResultTypeI t m args :: Maybe Symbol
  rawInvokeI :: (BridgeType t) -> CurryT (BridgeTypes args) (IO (BridgeTypeM (ResultTypeI t m args)))


invokeS :: forall m t args args'. ( Resolve t m args' ~ args
                                  , MethodS t m args
                                  , Marshal args' (BridgeTypes args)
                                  , Unmarshal (BridgeTypeM (ResultTypeS t m args)) (UnmarshalAs (BridgeTypeM (ResultTypeS t m args)))
                                  , Curry' (TupleSize args') ((BridgeTypes args) -> (IO (BridgeTypeM (ResultTypeS t m args)))) (CurryT (BridgeTypes args) (IO (BridgeTypeM (ResultTypeS t m args))))
                                  ) => args' -> IO (UnmarshalAs (BridgeTypeM (ResultTypeS t m args)))
invokeS x = marshal @args' @(BridgeTypes args) @((BridgeTypeM (ResultTypeS t m args))) x (\tup-> uncurryN' @(TupleSize args') (rawInvokeS @t @m @args) tup) >>= unmarshal

invokeI :: forall m t args args'. ( Resolve t m args' ~ args
                                  , MethodI t m args
                                  , Marshal args' (BridgeTypes args)
                                  , Marshal (Object t) (BridgeType t)
                                  , Unmarshal (BridgeTypeM (ResultTypeI t m args)) (UnmarshalAs (BridgeTypeM (ResultTypeI t m args)))
                                  , Curry' (TupleSize args') ((BridgeTypes args) -> (IO (BridgeTypeM (ResultTypeI t m args)))) (CurryT (BridgeTypes args) (IO (BridgeTypeM (ResultTypeI t m args))))
                                  ) => Object t -> args' -> IO (UnmarshalAs (BridgeTypeM (ResultTypeI t m args)))
invokeI obj x = marshal @args' @(BridgeTypes args) @((BridgeTypeM (ResultTypeI t m args))) x (\tup-> marshal @(Object t) @(BridgeType t) @((BridgeTypeM (ResultTypeI t m args))) obj (\obj'-> uncurryN' @(TupleSize args') (rawInvokeI @t @m @args obj') tup)) >>= unmarshal


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



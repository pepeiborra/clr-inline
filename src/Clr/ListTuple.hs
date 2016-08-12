{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, TypeOperators #-}

module Clr.ListTuple where

import Data.Kind
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

type family ListToTuple (t :: [Type]) :: Type where
  ListToTuple '[]                                 = ()
  ListToTuple (a ': '[])                         = a
  ListToTuple (a ': b ': '[])                   = (a,b)
  ListToTuple (a ': b ': c ': '[])             = (a,b,c)
  ListToTuple (a ': b ': c ': d ': '[])       = (a,b,c,d)
  ListToTuple (a ': b ': c ': d ': e ': '[]) = (a,b,c,d,e)

type family TupleSize (x::k) :: Nat where
  TupleSize (a,b,c,d) = 4
  TupleSize (a,b,c) = 3
  TupleSize (a,b) = 2
  TupleSize (a) = 1

type family Elem (a :: k) (xs::[k]) :: Bool where
  Elem a    '[]    = 'False
  Elem a (x ': xs) = a == x || (Elem a xs)

type family Concat (a::[t]) (b::[t]) :: [t] where
  Concat    '[]    ys  = ys
  Concat (x ': xs) ys  = x ': xs `Concat` ys

type family UnMaybeList (l :: [Maybe k]) :: [k] where
  UnMaybeList '[]             = '[]
  UnMaybeList ('Just x ': xs)  = x ': UnMaybeList xs
  UnMaybeList ('Nothing ': xs) = UnMaybeList xs



{-# LANGUAGE DataKinds, PolyKinds, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies, TypeOperators #-}

module Clr.ListTuple where

import Data.Kind
import Data.Type.Bool
import Data.Type.Equality
import GHC.TypeLits

--
-- List to tuple conversion
--
type family ListToTuple (t :: [Type]) :: Type where
  ListToTuple '[]                                 = ()
  ListToTuple (a ': '[])                         = (a)
  ListToTuple (a ': b ': '[])                   = (a,b)
  ListToTuple (a ': b ': c ': '[])             = (a,b,c)
  ListToTuple (a ': b ': c ': d ': '[])       = (a,b,c,d)
  ListToTuple (a ': b ': c ': d ': e ': '[]) = (a,b,c,d,e)

--
-- Size of a tuple
--
type family TupleSize (x::k) :: Nat where
  TupleSize (a,b,c,d) = 4
  TupleSize (a,b,c) = 3
  TupleSize (a,b) = 2
  TupleSize (a) = 1

--
-- Size of a list
--
type family ListSize (x::k) :: Nat where
  ListSize '[a,b,c,d] = 4
  ListSize '[a,b,c] = 3
  ListSize '[a,b] = 2
  ListSize '[a] = 1

--
-- a `Elem` xs is true if a is contained within xs
--
type family Elem (a :: k) (xs::[k]) :: Bool where
  Elem a    '[]    = 'False
  Elem a (x ': xs) = a == x || (Elem a xs)

--
-- Concatenation of 2 lists
--
type family Concat (a::[t]) (b::[t]) :: [t] where
  Concat    '[]    ys  = ys
  Concat (x ': xs) ys  = x ': xs `Concat` ys

--
-- Drops each Nothing from a list and does a fromJust on the others
--
type family CatMaybes (l :: [Maybe k]) :: [k] where
  CatMaybes '[]              = '[]
  CatMaybes ('Just x ': xs)  = x ': CatMaybes xs
  CatMaybes ('Nothing ': xs) = CatMaybes xs



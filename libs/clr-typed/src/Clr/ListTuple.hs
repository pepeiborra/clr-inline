{-# LANGUAGE TypeInType, PolyKinds, TypeFamilies, MultiParamTypeClasses #-}
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
  ListToTuple '[]                                                          = ()
  ListToTuple (a ': '[])                                                   = (a)
  ListToTuple (a ': b ': '[])                                              = (a,b)
  ListToTuple (a ': b ': c ': '[])                                         = (a,b,c)
  ListToTuple (a ': b ': c ': d ': '[])                                    = (a,b,c,d)
  ListToTuple (a ': b ': c ': d ': e ': '[])                               = (a,b,c,d,e)
  ListToTuple (a ': b ': c ': d ': e ': f ': '[])                          = (a,b,c,d,e,f)
  ListToTuple (a ': b ': c ': d ': e ': f ': g ': '[])                     = (a,b,c,d,e,f,g)
  ListToTuple (a ': b ': c ': d ': e ': f ': g ': h ': '[])                = (a,b,c,d,e,f,g,h)
  ListToTuple (a ': b ': c ': d ': e ': f ': g ': h ': i ': '[])           = (a,b,c,d,e,f,g,h,i)
  ListToTuple (a ': b ': c ': d ': e ': f ': g ': h ': i ': j ': '[])      = (a,b,c,d,e,f,g,h,i,j)
  ListToTuple (a ': b ': c ': d ': e ': f ': g ': h ': i ': j ': k ': '[]) = (a,b,c,d,e,f,g,h,i,j,k)

--
-- Tuple to list conversion
--
type family TupleToList (t::Type) :: [Type] where
  TupleToList ()                      = '[]
  TupleToList (a,b)                   = '[a, b]
  TupleToList (a,b,c)                 = '[a, b, c]
  TupleToList (a,b,c,d)               = '[a, b, c, d]
  TupleToList (a,b,c,d,e)             = '[a, b, c, d, e]
  TupleToList (a,b,c,d,e,f)           = '[a, b, c, d, e, f]
  TupleToList (a,b,c,d,e,f,g)         = '[a, b, c, d, e, f, g]
  TupleToList (a,b,c,d,e,f,g,h)       = '[a, b, c, d, e, f, g, h]
  TupleToList (a,b,c,d,e,f,g,h,i)     = '[a, b, c, d, e, f, g, h, i]
  TupleToList (a,b,c,d,e,f,g,h,i,j)   = '[a, b, c, d, e, f, g, h, i, j]
  TupleToList (a,b,c,d,e,f,g,h,i,j,k) = '[a, b, c, d, e, f, g, h, i, j, k]
  TupleToList (a)                     = '[a]  -- NB: "(a)" denotes anything that didn't match above, this could be either a non-tuple or a tuple bigger than what above could match on

--
-- Size of a tuple
--
type family TupleSize (x::tupleKind) :: Nat where
  TupleSize a = ListSize (TupleToList a) -- NB: the size of '()' is considered 0, and anything non tuple or bigger than what 'TupleToList' matches on is 1.

--
-- ArgCount is like TupleSize, except evaluates to 1 for '()', which only makes sens given how it is currently used
--
type family ArgCount (x::tupleKind) :: Nat where
  ArgCount () = 1
  ArgCount a  = TupleSize a

--
-- Size of a list
--
type family ListSize (x::k) :: Nat where
  ListSize    '[]    = 0
  ListSize (x ': xs) = 1 + (ListSize xs)

--
-- a `Elem` xs is true if a is contained within xs
--
type family Elem (a :: k) (xs::[k]) :: Bool where
  Elem a    '[]    = 'False
  Elem a (x ': xs) = a == x || (Elem a xs)

--
-- Concatenate a type level list of lists
--
type family Concat (a::[[t]]) :: [t] where
  Concat   '[]     = '[]
  Concat (x ': xs) = x `Append` (Concat xs)

--
-- Append 2 type level lists
--
type family Append (a::[t]) (b::[t]) :: [t] where
  Append    '[]    ys  = ys
  Append (x ': xs) ys  = x ': (xs `Append` ys)

--
-- Drops each Nothing from a list and does a fromJust on the others
--
type family CatMaybes (l :: [Maybe k]) :: [k] where
  CatMaybes '[]              = '[]
  CatMaybes ('Just x ': xs)  = x ': CatMaybes xs
  CatMaybes ('Nothing ': xs) = CatMaybes xs

--
-- PrependIf b x xs evaluates to xs when b is false and x : xs when b is true
--
type family PrependIf (b :: Bool) (x :: k) (xs :: [k]) :: [k] where
  PrependIf 'True  x xs = x ': xs
  PrependIf 'False x xs = xs


type family Index (x::[t]) (n::Nat) :: t where
  Index (x ': xs) 0 = x
  Index (x ': xs) n = Index xs (n-1)
  Index     xs    n = TypeError (Text "Out of bounds")


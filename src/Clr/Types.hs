{-# LANGUAGE KindSignatures, TypeInType, TypeFamilies, TypeOperators #-}

module Clr.Types where

import Data.Kind
import GHC.TypeLits

--
-- T is the Haskell representation of a type from the CLR, which is a symbol to represent the
-- name, plus zero or more other instances of T that it is instantiated with to implement generics.
--
data T (name::Symbol) (genArgs::[Type])

--
-- MakeT is used to simplify the API so that a Symbol can be used as shorthand to construct a T when
-- generics are not involved, and a promoted tuple of '(Symbol, (Symbol or T)) as shorthand for when they are.
--
type family MakeT (x::k) :: Type where
  MakeT (T name gt)    = T name gt              -- A fully constructed CLR type representation evaluates to unchanged
  MakeT (name::Symbol) = T name '[]             -- A symbol is shorthand for a non-generic type
  MakeT '(name, g1)    = T name '[MakeT g1]     -- A tuple of size n is shorthand for a generic type instantiated with n-1 other types
  MakeT '(name, g1, g2)  = T name '[MakeT g1, MakeT g2]
  MakeT '(name, g1, g2, g3) = T name '[MakeT g1, MakeT g2, MakeT g3]
  MakeT '(name, g1, g2, g3, g4) = T name '[MakeT g1, MakeT g2, MakeT g3, MakeT g4]
  MakeT '(name, g1, g2, g3, g4, g5) = T name '[MakeT g1, MakeT g2, MakeT g3, MakeT g4, MakeT g5]
  MakeT '(name, g1, g2, g3, g4, g5, g6) = T name '[MakeT g1, MakeT g2, MakeT g3, MakeT g4, MakeT g5, MakeT g6]
  MakeT '(name, g1, g2, g3, g4, g5, g6, g7) = T name '[MakeT g1, MakeT g2, MakeT g3, MakeT g4, MakeT g5, MakeT g6, MakeT g7]
  MakeT '(name, g1, g2, g3, g4, g5, g6, g7, g8) = T name '[MakeT g1, MakeT g2, MakeT g3, MakeT g4, MakeT g5, MakeT g6, MakeT g7, MakeT g8]
  MakeT '(name, g1, g2, g3, g4, g5, g6, g7, g8, g9) = T name '[MakeT g1, MakeT g2, MakeT g3, MakeT g4, MakeT g5, MakeT g6, MakeT g7, MakeT g8, MakeT g9]
  MakeT '(name, g1, g2, g3, g4, g5, g6, g7, g8, g9, g10) = T name '[MakeT g1, MakeT g2, MakeT g3, MakeT g4, MakeT g5, MakeT g6, MakeT g7, MakeT g8, MakeT g9, MakeT g10]

--
-- Prim types are all values types + Sytem.String
--
type family IsPrimType (a::Type) :: Bool where
  IsPrimType (T "System.String" '[])  = 'True
  IsPrimType (T "System.Int16" '[])   = 'True
  IsPrimType (T "System.UInt16" '[])  = 'True
  IsPrimType (T "System.Int32" '[])   = 'True
  IsPrimType (T "System.UInt32" '[])  = 'True
  IsPrimType (T "System.Int64" '[])   = 'True
  IsPrimType (T "System.UInt64" '[])  = 'True
  IsPrimType (T "System.IntPtr" '[])  = 'True
  IsPrimType (T "System.UIntPtr" '[]) = 'True
  IsPrimType (T "System.Char" '[])    = 'True
  IsPrimType (T "System.Single" '[])  = 'True
  IsPrimType (T "System.Double" '[])  = 'True
  IsPrimType t = 'False


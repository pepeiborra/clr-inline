{-# LANGUAGE KindSignatures, TypeInType, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Clr.Types where

import Clr.ListTuple

import Data.Int
import Data.Kind
import Data.Word
import Foreign.Ptr
import GHC.TypeLits

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

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
  MakeT (T name gt)    = T name gt          -- A fully constructed CLR type representation evaluates to itself unchanged
  MakeT (name::Symbol) = T name '[]         -- A symbol is shorthand for a non-generic type
  MakeT '(name, g1)    = T name '[MakeT g1] -- A tuple of size n is shorthand for a generic type instantiated with n-1 other types
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
-- Retrieve the name component of a CLR type representation T
--
type family T_GetName (t::Type) :: Symbol where
  T_GetName (T name gt) = name

--
-- Retrieve the generic type param component of a CLR type representation T
--
type family T_GetGenT (t::Type) :: [Type] where
  T_GetGenT (T name gt) = gt

--
-- Prim types are all values types plus Sytem.String.
-- These are the types that have their own bridge type, everything else is an object reference.
--
type PrimTypes = '[ T "System.String"  '[]
                  , T "System.Boolean" '[]
                  , T "System.SByte"   '[]
                  , T "System.Byte"    '[]
                  , T "System.Int16"   '[]
                  , T "System.UInt16"  '[]
                  , T "System.Int32"   '[]
                  , T "System.UInt32"  '[]
                  , T "System.Int64"   '[]
                  , T "System.UInt64"  '[]
                  , T "System.IntPtr"  '[]
                  , T "System.UIntPtr" '[]
                  , T "System.Char"    '[]
                  , T "System.Single"  '[]
                  , T "System.Double"  '[] ]

type family IsPrimType (t::Type) :: Bool where
  IsPrimType t = t `Elem` PrimTypes

--
-- HaskToClr maps a native Haskell type to the corresponding CLR type.
-- This is an initial stage before the resolver figures out the best implicit
-- conversions to use & overload selection.
type family HaskToClr (h::Type) :: Type

type instance HaskToClr String  = T "System.String"  '[]
type instance HaskToClr T.Text  = T "System.String"  '[]
type instance HaskToClr TL.Text = T "System.String"  '[]

type instance HaskToClr Bool    = T "System.Boolean" '[]
type instance HaskToClr Int8    = T "System.SByte"   '[]
type instance HaskToClr Word8   = T "System.Byte"    '[]
type instance HaskToClr Int16   = T "System.Int16"   '[]
type instance HaskToClr Word16  = T "System.UInt16"  '[]
type instance HaskToClr Int32   = T "System.Int32"   '[]
type instance HaskToClr Word32  = T "System.UInt32"  '[]
type instance HaskToClr Int64   = T "System.Int64"   '[]
type instance HaskToClr Word64  = T "System.UInt64"  '[]
type instance HaskToClr IntPtr  = T "System.IntPtr"  '[]
type instance HaskToClr WordPtr = T "System.UIntPtr" '[]
type instance HaskToClr Char    = T "System.Char"    '[]
type instance HaskToClr Float   = T "System.Single"  '[]
type instance HaskToClr Double  = T "System.Double"  '[]

type family HaskToClrL (l::[Type]) :: [Type] where
  HaskToClrL   '[]     = '[]
  HaskToClrL (x ': xs) = (HaskToClr x) ': (HaskToClrL xs)

--
-- C# style synonyms
--
type T_sbyte   = T "System.SByte"   '[]
type T_byte    = T "System.Byte"    '[]
type T_short   = T "System.Int16"   '[]
type T_ushort  = T "System.UInt16"  '[]
type T_int     = T "System.Int32"   '[]
type T_uint    = T "System.UInt32"  '[]
type T_long    = T "System.Int64"   '[]
type T_ulong   = T "System.UInt64"  '[]
type T_float   = T "System.Single"  '[]
type T_double  = T "System.Double"  '[]
type T_decimal = T "System.Decimal" '[]
type T_object  = T "System.Object"  '[]
type T_string  = T "System.String"  '[]
type T_char    = T "System.Char"    '[]
type T_bool    = T "System.Boolean" '[]

--
-- Declaration of all compile type chooseable members of a particular type
--
type family Members (t::Type) :: [Type]

--
-- t `HasMember` m ~ True t has declared m within its members
--
type family HasMember (t::Type) (m::Type) :: Bool where
  HasMember t m = m `Elem` (Members t)


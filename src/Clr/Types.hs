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
-- name, the array elment type if applicable, plus zero or more other instances of T that it
-- is instantiated with to implement generics.
--
data T (name::Symbol) (element::Maybe Type) (genArgs::[Type])

--
-- MakeT is used to simplify the API so that a Symbol can be used as shorthand to construct a T when
-- generics are not involved, and a promoted tuple of '(Symbol, (Symbol or T)) as shorthand for when they are.
--
type family MakeT (x::k) :: Type where
  MakeT (T name element gt) = T name element gt       -- A fully constructed CLR type representation evaluates to itself unchanged
  MakeT (name::Symbol)      = T name 'Nothing '[]     -- A symbol is shorthand for a non-generic type
  MakeT '(name, g1) = T name 'Nothing '[MakeT g1]     -- A tuple of size n is shorthand for a generic type instantiated with n-1 other types
  MakeT '(name, g1, g2)  = T name 'Nothing '[MakeT g1, MakeT g2]
  MakeT '(name, g1, g2, g3) = T name 'Nothing '[MakeT g1, MakeT g2, MakeT g3]
  MakeT '(name, g1, g2, g3, g4) = T name 'Nothing '[MakeT g1, MakeT g2, MakeT g3, MakeT g4]
  MakeT '(name, g1, g2, g3, g4, g5) = T name 'Nothing '[MakeT g1, MakeT g2, MakeT g3, MakeT g4, MakeT g5]
  MakeT '(name, g1, g2, g3, g4, g5, g6) = T name 'Nothing '[MakeT g1, MakeT g2, MakeT g3, MakeT g4, MakeT g5, MakeT g6]
  MakeT '(name, g1, g2, g3, g4, g5, g6, g7) = T name 'Nothing '[MakeT g1, MakeT g2, MakeT g3, MakeT g4, MakeT g5, MakeT g6, MakeT g7]
  MakeT '(name, g1, g2, g3, g4, g5, g6, g7, g8) = T name 'Nothing '[MakeT g1, MakeT g2, MakeT g3, MakeT g4, MakeT g5, MakeT g6, MakeT g7, MakeT g8]
  MakeT '(name, g1, g2, g3, g4, g5, g6, g7, g8, g9) = T name 'Nothing '[MakeT g1, MakeT g2, MakeT g3, MakeT g4, MakeT g5, MakeT g6, MakeT g7, MakeT g8, MakeT g9]
  MakeT '(name, g1, g2, g3, g4, g5, g6, g7, g8, g9, g10) = T name 'Nothing '[MakeT g1, MakeT g2, MakeT g3, MakeT g4, MakeT g5, MakeT g6, MakeT g7, MakeT g8, MakeT g9, MakeT g10]

type family MakeArrT (x::k) :: Type where
  MakeArrT (T name element gt) = T "System.Array" ('Just (T name element gt)) '[]  -- A fully constructed CLR type representation evaluates to the array of that type
  MakeArrT   element           = T "System.Array" ('Just (MakeT element))     '[]  -- A symbol or tuple is shorthand for the array of that type

--
-- Prim types are all values types plus Sytem.String.
-- These are the types that have their own bridge type, everything else is an object reference.
--
type PrimTypes = '[ T "System.String"  'Nothing '[]
                  , T "System.SByte"   'Nothing '[]
                  , T "System.Byte"    'Nothing '[]
                  , T "System.Int16"   'Nothing '[]
                  , T "System.UInt16"  'Nothing '[]
                  , T "System.Int32"   'Nothing '[]
                  , T "System.UInt32"  'Nothing '[]
                  , T "System.Int64"   'Nothing '[]
                  , T "System.UInt64"  'Nothing '[]
                  , T "System.IntPtr"  'Nothing '[]
                  , T "System.UIntPtr" 'Nothing '[]
                  , T "System.Char"    'Nothing '[]
                  , T "System.Single"  'Nothing '[]
                  , T "System.Double"  'Nothing '[] ]

type family IsPrimType (t::Type) :: Bool where
  IsPrimType t = t `Elem` PrimTypes

--
-- HaskToClr maps a native Haskell type to the corresponding CLR type.
-- This is an initial stage before the resolver figures out the best implicit
-- conversions to use & overload selection.
type family HaskToClr (h::Type) :: Type

type instance HaskToClr String  = T "System.String"  'Nothing '[]
type instance HaskToClr T.Text  = T "System.String"  'Nothing '[]
type instance HaskToClr TL.Text = T "System.String"  'Nothing '[]

type instance HaskToClr Int8    = T "System.SByte"   'Nothing '[]
type instance HaskToClr Word8   = T "System.Byte"    'Nothing '[]
type instance HaskToClr Int16   = T "System.Int16"   'Nothing '[]
type instance HaskToClr Word16  = T "System.UInt16"  'Nothing '[]
type instance HaskToClr Int32   = T "System.Int32"   'Nothing '[]
type instance HaskToClr Word32  = T "System.UInt32"  'Nothing '[]
type instance HaskToClr Int64   = T "System.Int64"   'Nothing '[]
type instance HaskToClr Word64  = T "System.UInt64"  'Nothing '[]
type instance HaskToClr IntPtr  = T "System.IntPtr"  'Nothing '[]
type instance HaskToClr WordPtr = T "System.UIntPtr" 'Nothing '[]
type instance HaskToClr Char    = T "System.Char"    'Nothing '[]
type instance HaskToClr Float   = T "System.Single"  'Nothing '[]
type instance HaskToClr Double  = T "System.Double"  'Nothing '[]

type family HaskToClrL (l::[Type]) :: [Type] where
  HaskToClrL   '[]     = '[]
  HaskToClrL (x ': xs) = (HaskToClr x) ': (HaskToClrL xs)

--
-- C# style synonyms
--
type T_sbyte   = T "System.SByte"   'Nothing '[]
type T_byte    = T "System.Byte"    'Nothing '[]
type T_short   = T "System.Int16"   'Nothing '[]
type T_ushort  = T "System.UInt16"  'Nothing '[]
type T_int     = T "System.Int32"   'Nothing '[]
type T_uint    = T "System.UInt32"  'Nothing '[]
type T_long    = T "System.Int64"   'Nothing '[]
type T_ulong   = T "System.UInt64"  'Nothing '[]
type T_float   = T "System.Single"  'Nothing '[]
type T_double  = T "System.Double"  'Nothing '[]
type T_decimal = T "System.Decimal" 'Nothing '[]
type T_object  = T "System.Object"  'Nothing '[]
type T_string  = T "System.String"  'Nothing '[]
type T_char    = T "System.Char"    'Nothing '[]
type T_bool    = T "System.Boolean" 'Nothing '[]


{-# LANGUAGE KindSignatures, TypeInType, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Clr.Resolver.ImplicitConversions (ImplicitConvExists) where

import Clr.ListTuple
import Clr.Types
import Clr.Inheritance
import Clr.Interface

import Data.Kind
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality

--
-- ImplicitConvExists x y is true if there is an implicit conversion
-- from x to y as defined by ECMA-364 section 13.1
--
type family ImplicitConvExists (x::Type) (y::Type) :: Bool where
  ImplicitConvExists t  t  = 'True -- identity conversion
  ImplicitConvExists t1 t2 = ImpNumConvExists   t1 t2
                          || ImpEnumConvExists  t1 t2
                          || ImpRefConvExists   t1 t2
                          || ImpBoxConvExists   t1 t2
                          || ImpConstConvExists t1 t2

type family ImpNumConvExists (x::Type) (y::Type) :: Bool where
  ImpNumConvExists T_sbyte  T_short   = 'True
  ImpNumConvExists T_sbyte  T_int     = 'True
  ImpNumConvExists T_sbyte  T_long    = 'True
  ImpNumConvExists T_sbyte  T_float   = 'True
  ImpNumConvExists T_sbyte  T_double  = 'True
  ImpNumConvExists T_sbyte  T_decimal = 'True
  ImpNumConvExists T_byte   T_short   = 'True
  ImpNumConvExists T_byte   T_ushort  = 'True
  ImpNumConvExists T_byte   T_int     = 'True
  ImpNumConvExists T_byte   T_uint    = 'True
  ImpNumConvExists T_byte   T_long    = 'True
  ImpNumConvExists T_byte   T_ulong   = 'True
  ImpNumConvExists T_byte   T_float   = 'True
  ImpNumConvExists T_byte   T_double  = 'True
  ImpNumConvExists T_byte   T_decimal = 'True
  ImpNumConvExists T_short  T_int     = 'True
  ImpNumConvExists T_short  T_long    = 'True
  ImpNumConvExists T_short  T_float   = 'True
  ImpNumConvExists T_short  T_double  = 'True
  ImpNumConvExists T_short  T_decimal = 'True
  ImpNumConvExists T_ushort T_int     = 'True
  ImpNumConvExists T_ushort T_uint    = 'True
  ImpNumConvExists T_ushort T_long    = 'True
  ImpNumConvExists T_ushort T_ulong   = 'True
  ImpNumConvExists T_ushort T_float   = 'True
  ImpNumConvExists T_ushort T_double  = 'True
  ImpNumConvExists T_ushort T_decimal = 'True
  ImpNumConvExists T_int    T_long    = 'True
  ImpNumConvExists T_int    T_float   = 'True
  ImpNumConvExists T_int    T_double  = 'True
  ImpNumConvExists T_int    T_decimal = 'True
  ImpNumConvExists T_uint   T_long    = 'True
  ImpNumConvExists T_uint   T_ulong   = 'True
  ImpNumConvExists T_uint   T_float   = 'True
  ImpNumConvExists T_uint   T_double  = 'True
  ImpNumConvExists T_uint   T_decimal = 'True
  ImpNumConvExists T_long   T_float   = 'True
  ImpNumConvExists T_long   T_double  = 'True
  ImpNumConvExists T_long   T_decimal = 'True
  ImpNumConvExists T_ulong  T_float   = 'True
  ImpNumConvExists T_ulong  T_double  = 'True
  ImpNumConvExists T_ulong  T_decimal = 'True
  ImpNumConvExists T_char   T_ushort  = 'True
  ImpNumConvExists T_char   T_int     = 'True
  ImpNumConvExists T_char   T_uint    = 'True
  ImpNumConvExists T_char   T_long    = 'True
  ImpNumConvExists T_char   T_ulong   = 'True
  ImpNumConvExists T_char   T_float   = 'True
  ImpNumConvExists T_char   T_double  = 'True
  ImpNumConvExists T_char   T_decimal = 'True
  ImpNumConvExists T_float  T_double  = 'True
  ImpNumConvExists t1       t2        = 'False

-- TODO
type family ImpEnumConvExists (x::Type) (y::Type) :: Bool where
  ImpEnumConvExists t1 t2 = 'False

-- TODO: more complicated than this see 13.1.4
type family ImpRefConvExists (x::Type) (y::Type) :: Bool where
  ImpRefConvExists t1 t2 = t1 `Implements` t2 || t1 `InheritsFrom` t2

-- TODO: probably handled by above though
type family ImpBoxConvExists (x::Type) (y::Type) :: Bool where
  ImpBoxConvExists t1 t2 = 'False

-- TODO
type family ImpConstConvExists (x::Type) (y::Type) :: Bool where
  ImpConstConvExists t1 t2 = 'False



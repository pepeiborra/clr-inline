{-# LANGUAGE KindSignatures, TypeInType, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Clr.Resolver.BetterConversion (IsBetterConv) where

import Clr.Resolver.ImplicitConversions
import Clr.ListTuple
import Clr.Types

import Data.Kind
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality

--
-- Implements Better conversion as defined in section 14.4.2.3
-- of ECMA-364
--
type family IsBetterConv (s::Type) (t1::Type) (t2::Type) :: Bool where
  IsBetterConv s t1 t2 = BestType s t1 t2 == 'Just t1

type family BestType (s::Type) (t1::Type) (t2::Type) :: Maybe Type where
  BestType s  t  t  = 'Nothing
  BestType t1 t1 t2 = 'Just t1
  BestType t2 t1 t2 = 'Just t2
  BestType s  t1 t2 = BestType' (ImplicitConvExists t1 t2) t1 (ImplicitConvExists t2 t1) t2

type family BestType' (b1::Bool) (t1::Type) (b2::Bool) (t2::Type) :: Maybe Type where
  BestType' 'True  t1 'False t2 = 'Just t1
  BestType' 'False t1 'True  t2 = 'Just t2
  BestType'  _     t1  _     t2 = (BestType'' t1 t2) <|> (BestType'' t2 t1)

type family BestType'' (t1::Type) (t2::Type) :: Maybe Type where
  BestType'' (T "System.SByte" 'Nothing '[]) t2 = If (t2 `Elem` UNumAtLeastSByte) ('Just (T "System.SByte" 'Nothing '[])) 'Nothing
  BestType'' (T "System.Int16" 'Nothing '[]) t2 = If (t2 `Elem` UNumAtLeastInt16) ('Just (T "System.Int16" 'Nothing '[])) 'Nothing
  BestType'' (T "System.Int32" 'Nothing '[]) t2 = If (t2 `Elem` UNumAtLeastInt32) ('Just (T "System.Int32" 'Nothing '[])) 'Nothing
  BestType'' (T "System.Int64" 'Nothing '[]) t2 = If (t2 `Elem` UNumAtLeastInt64) ('Just (T "System.Int64" 'Nothing '[])) 'Nothing
  BestType''         t1             t2 = 'Nothing


type family (a::k) <|> (b::k) :: k

type family MaybeAlt (a::Maybe k) (b::Maybe k) :: Maybe k where
  MaybeAlt 'Nothing     x    = x
  MaybeAlt    x     'Nothing = x

type instance a <|> b = MaybeAlt a b

type UNumAtLeastSByte = (T "System.Byte" 'Nothing '[]) ': UNumAtLeastInt32
type UNumAtLeastInt16 = (T "System.UInt16" 'Nothing '[]) ': UNumAtLeastInt32
type UNumAtLeastInt32 = (T "System.UInt32" 'Nothing '[]) ': UNumAtLeastInt64
type UNumAtLeastInt64 = '[ T "System.UInt64" 'Nothing '[] ]


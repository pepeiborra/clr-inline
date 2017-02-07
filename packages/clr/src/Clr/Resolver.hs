{-# LANGUAGE KindSignatures, TypeInType, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Clr.Resolver(Candidates, ResolveMember) where

import Clr.Resolver.BetterConversion
import Clr.Resolver.ImplicitConversions
import Clr.ListTuple
import Clr.Object
import Clr.Types

import Data.Kind
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality

type family Candidates (typ::Type) (member::Type) :: [[Type]]

type family ResolveMember (as::[Type]) (ms::[[Type]]) :: [Type] where
  ResolveMember as ms = ResolveMember' as (FilterApp as ms)

type family ResolveMember' (as::[Type]) (fa::[[Type]]) :: [Type] where
  ResolveMember' as fa = ResolveMember'' (FilterBestMembers as fa fa)

type family ResolveMember'' (bestMember::[[Type]]) :: [Type] where
  ResolveMember'' (member ': '[]) = member
  ResolveMember''    _            = TypeError (Text "Could not resolve")

type family FilterBestMembers (as::[Type]) (ms::[[Type]]) (ns::[[Type]]) :: [[Type]] where
  FilterBestMembers as ms '[]       = '[]
  FilterBestMembers as ms (n ': ns) = FilterBestMembers' as ms n (FilterBestMembers as ms ns)

type family FilterBestMembers' (as::[Type]) (ms::[[Type]]) (n :: [Type] ) (fbms::[[Type]]) :: [[Type]] where
  FilterBestMembers' as ms n fbms = PrependIf (IsBestMember as ms n) n fbms

type family IsBestMember (as::[Type]) (ms::[[Type]]) (n :: [Type]) :: Bool where
  IsBestMember as '[]       n = True
  IsBestMember as (m ': ms) n = If (m == n)
                                  (IsBestMember as ms n)
                                  ((IsBetterMember as n m) && (IsBestMember as ms n))

--
-- IsBetterMember as ps qs is true if member ps is better than member qs with respect
-- to the arguments supplied as
--
type family IsBetterMember (as::[Type]) (ps::[Type]) (qs::[Type]) :: Bool where
  IsBetterMember as ps qs = (AnyBetterConv as ps qs) && (Not (AnyBetterConv as qs ps))

type family AnyBetterConv (as::[Type]) (ps::[Type]) (qs::[Type]) :: Bool where
  AnyBetterConv  '[]        '[]        '[]        = 'False
  AnyBetterConv  (a ': as)  (p ': ps)  (q ': qs)  = (IsBetterConv a p q) || (AnyBetterConv as ps qs)


type family FilterApp (as::[Type]) (ms::[[Type]]) :: [[Type]] where
  FilterApp as '[]       = '[]
  FilterApp as (m ': ms) = FilterApp' as m (FilterApp as ms)

type family FilterApp'(as::[Type]) (m :: [Type]) (fas::[[Type]]) :: [[Type]] where
  FilterApp' as m fas = PrependIf (IsApp as m) m fas

type family IsApp as ps :: Bool where
  IsApp '[]       '[]       = True
  IsApp '[]       (p ': ps) = False -- different lengths
  IsApp (a ': as) '[]       = False -- different lengths
  IsApp (a ': as) (p ': ps) = (ImplicitConvExists a p) && (IsApp as ps)


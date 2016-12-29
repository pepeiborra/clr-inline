{-# LANGUAGE KindSignatures, TypeInType, TypeFamilies, TypeOperators, UndecidableInstances #-}

module Clr.Resolver where

import Clr.Resolver.BetterConversion
import Clr.ListTuple
import Clr.Object
import Clr.Types

import Data.Kind
import GHC.TypeLits
import Data.Type.Bool
import Data.Type.Equality



type family IsBestMember (as::[Type]) (ms::[[Type]]) (n :: [Type]) :: Bool where
  IsBestMember as '[]       n = True
  IsBestMember as (m ': ms) n = If (m == n)
                                  (IsBestMember as ms n)
                                  ((IsBetterMember as n m) && (IsBestMember as ms n))

--
-- IsBetterMember as ps qs is true if ps is better than qs
type family IsBetterMember (as::[Type]) (ps::[Type]) (qs::[Type]) :: Bool where
  IsBetterMember as ps qs = (AnyBetterConv as ps qs) && (Not (AnyBetterConv as qs ps))

type family AnyBetterConv (as::[Type]) (ps::[Type]) (qs::[Type]) :: Bool where
  AnyBetterConv  '[]        '[]        '[]        = 'False
  AnyBetterConv  (a ': as)  (p ': ps)  (q ': qs)  = (IsBetterConv a p q) || (AnyBetterConv as ps qs)




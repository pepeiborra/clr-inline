{-# LANGUAGE KindSignatures, TypeInType, TypeFamilies, TypeOperators, UndecidableInstances, RankNTypes, MultiParamTypeClasses, AllowAmbiguousTypes, FlexibleInstances, TypeApplications, ScopedTypeVariables, MagicHash #-}

module Clr.TypeString where

import Clr.ListTuple
import Clr.Types

import Data.List(intercalate)
import Data.Kind
import GHC.TypeLits
import GHC.Prim

--
-- TString is used to get the runtime value string of a type
--
class TString t where
  tString :: forall t . String

--
-- TString instance for (T name gt)
--
instance (KnownSymbol name, TString gt) => TString (T name gt) where
  tString = symbolVal' (proxy# :: Proxy# name) ++ (tString @gt)

--
-- TString instance for unit (used as the argument for CLR methods that don't otherwise have arguments)
--
instance TString () where
  tString = ""

--
-- TString instance for empty list (used to mean no generic type parameter instantiations)
--
instance TString '[] where
  tString = ""

--
-- TSting instance for non-empty list (used for generic type parameter instantiations)
--
instance (TString x, TStrings xs, (KnownNat (1 + ListSize xs))) => TString (x ': xs) where
  tString = "`" ++ show (natVal' (proxy# :: Proxy# (1 + ListSize xs))) ++ "[" ++ intercalate "," (tString @x : tStrings @xs) ++ "]"

--
-- TStrings is like TString except it evaluates to a list of strings
--
class TStrings ts where
  tStrings :: forall gt' . [String]

instance TStrings '[] where
  tStrings = []

instance (TString x, TStrings xs) => TStrings (x ': xs) where
  tStrings = tString @x : tStrings @xs

--
-- Property getter / setter methods
--
tStringGet :: forall t . (TString t) => String
tStringGet = "get_" ++ tString @t

tStringSet :: forall t . (TString t) => String
tStringSet = "set_" ++ tString @t


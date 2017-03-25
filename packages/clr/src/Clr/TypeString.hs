{-# LANGUAGE KindSignatures, TypeInType, TypeFamilies, TypeOperators, UndecidableInstances, RankNTypes, MultiParamTypeClasses, AllowAmbiguousTypes, FlexibleInstances, TypeApplications, ScopedTypeVariables, MagicHash #-}

module Clr.TypeString (stringifyT) where

import Clr.ListTuple
import Clr.Types

import Data.Kind
import GHC.TypeLits
import GHC.Prim

--
-- TString is used in the implmentation of stringifyT. There's an instance for each count of generic type param instantiations
--
class TString name gt where
  tString :: forall name gt . String

--
-- TString instance for a non generic type
--
instance (KnownSymbol name) => TString name '[] where
  tString = symbolVal' (proxy# :: Proxy# name)

--
-- TString instance for a type instantiated with 1 generic type parameter
--
instance ( KnownSymbol name
         , T_GetName x1 ~ x1Name
         , T_GetGenT x1 ~ x1GT
         , TString x1Name x1GT
         ) => TString name (x1 ': '[]) where
  tString = symbolVal' (proxy# :: Proxy# name) ++ "`1[" ++ tString @x1Name @x1GT ++ "]"

--
-- TString instance for a type instantiated with 2 generic type parameters
--
instance ( KnownSymbol name
         , T_GetName x1 ~ x1Name
         , T_GetGenT x1 ~ x1GT
         , TString x1Name x1GT
         , T_GetName x2 ~ x2Name
         , T_GetGenT x2 ~ x2GT
         , TString x2Name x2GT
         ) => TString name (x1 ': x2 ': '[]) where
  tString = symbolVal' (proxy# :: Proxy# name) ++ "`2[" ++ tString @x1Name @x1GT ++ "," ++ tString @x2Name @x2GT ++ "]"

--
-- TString instance for a type instantiated with 3 generic type parameters
--
instance ( KnownSymbol name
         , T_GetName x1 ~ x1Name
         , T_GetGenT x1 ~ x1GT
         , TString x1Name x1GT
         , T_GetName x2 ~ x2Name
         , T_GetGenT x2 ~ x2GT
         , TString x2Name x2GT
         , T_GetName x3 ~ x3Name
         , T_GetGenT x3 ~ x3GT
         , TString x3Name x3GT
         ) => TString name (x1 ': x2 ': x3 ': '[]) where
  tString = symbolVal' (proxy# :: Proxy# name) ++ "`3[" ++ tString @x1Name @x1GT ++ "," ++ tString @x2Name @x2GT ++ "," ++ tString @x3Name @x3GT ++ "]"

--
-- TString instance for a type instantiated with 4 generic type parameters
--
instance ( KnownSymbol name
         , T_GetName x1 ~ x1Name
         , T_GetGenT x1 ~ x1GT
         , TString x1Name x1GT
         , T_GetName x2 ~ x2Name
         , T_GetGenT x2 ~ x2GT
         , TString x2Name x2GT
         , T_GetName x3 ~ x3Name
         , T_GetGenT x3 ~ x3GT
         , TString x3Name x3GT
         , T_GetName x4 ~ x4Name
         , T_GetGenT x4 ~ x4GT
         , TString x4Name x4GT
         ) => TString name (x1 ': x2 ': x3 ': x4 ': '[]) where
  tString = symbolVal' (proxy# :: Proxy# name) ++ "`4[" ++ tString @x1Name @x1GT ++ "," ++ tString @x2Name @x2GT ++ "," ++ tString @x3Name @x3GT ++ "," ++ tString @x4Name @x4GT ++ "]"

--
-- TString instance for a type instantiated with 5 generic type parameters
--
instance ( KnownSymbol name
         , T_GetName x1 ~ x1Name
         , T_GetGenT x1 ~ x1GT
         , TString x1Name x1GT
         , T_GetName x2 ~ x2Name
         , T_GetGenT x2 ~ x2GT
         , TString x2Name x2GT
         , T_GetName x3 ~ x3Name
         , T_GetGenT x3 ~ x3GT
         , TString x3Name x3GT
         , T_GetName x4 ~ x4Name
         , T_GetGenT x4 ~ x4GT
         , TString x4Name x4GT
         , T_GetName x5 ~ x5Name
         , T_GetGenT x5 ~ x5GT
         , TString x5Name x5GT
         ) => TString name (x1 ': x2 ': x3 ': x4 ': x5 ': '[]) where
  tString = symbolVal' (proxy# :: Proxy# name) ++ "`5[" ++ tString @x1Name @x1GT ++ "," ++ tString @x2Name @x2GT ++ "," ++ tString @x3Name @x3GT ++ "," ++ tString @x4Name @x4GT ++ "," ++ tString @x5Name @x5GT ++ "]"

--
-- stringifyT is used to get the runtime string identifier (as used in the CLR) for the supplied Haskell type level representation of a CLR type
-- Eg:  stringifyT @"System.String"                                             = "System.String"
--      stringifyT @'("System.Collections.Generic.IEnumerable", "System.Int32") = "System.Collections.Generic.IEnumerable`1[System.Int32]
--
stringifyT :: forall ts t name gt . (MakeT ts ~ t, T_GetName t ~ name, T_GetGenT t ~ gt, TString name gt) => String
stringifyT = tString @name @gt


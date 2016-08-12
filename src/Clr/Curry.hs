{-# LANGUAGE DataKinds, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies #-}

module Clr.Curry where

import Data.Tuple.Curry
import GHC.TypeLits

type family CurryT x r where
  CurryT (a,b,c,d) r = a -> b -> c -> d -> r
  CurryT (a,b,c) r = a -> b -> c -> r
  CurryT (a,b) r = a -> b -> r
  CurryT a r = a -> r

class Curry' (n::Nat) a b | n b -> a, n a -> b where
  curryN' :: a -> b
  uncurryN' :: b -> a

instance Curry' 1 (a -> r) (a -> r) where
  curryN' = id
  uncurryN' = id

instance Curry' 2 ((a,b) -> r) (a -> b -> r)  where
  curryN' = curryN
  uncurryN' = uncurryN

instance Curry' 3 ((a,b,c) -> r) (a -> b -> c -> r)  where
  curryN' = curryN
  uncurryN' = uncurryN

instance Curry' 4 ((a,b,c,d) -> r) (a -> b -> c -> d -> r)  where
  curryN' = curryN
  uncurryN' = uncurryN


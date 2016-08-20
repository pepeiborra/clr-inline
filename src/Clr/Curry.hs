{-# LANGUAGE DataKinds, TypeFamilies, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE AllowAmbiguousTypes, FunctionalDependencies #-}

module Clr.Curry where

import qualified Data.Tuple.Curry as C
import GHC.TypeLits

--
-- A curried type transformation
--
type family CurryT x r where
  CurryT (a,b,c,d) r = a -> b -> c -> d -> r
  CurryT (a,b,c) r = a -> b -> c -> r
  CurryT (a,b) r = a -> b -> r
  CurryT a r = a -> r

--
-- Curry is like that from the tuple package, except that we use id
-- instead of OneTuple. This is achieved with an extra type param of
-- the tuple size.
class Curry (n::Nat) a b | n b -> a, n a -> b where
  curryN :: a -> b
  uncurryN :: b -> a

instance Curry 1 (a -> r) (a -> r) where
  curryN = id
  uncurryN = id

instance Curry 2 ((a,b) -> r) (a -> b -> r)  where
  curryN = C.curryN
  uncurryN = C.uncurryN

instance Curry 3 ((a,b,c) -> r) (a -> b -> c -> r)  where
  curryN = C.curryN
  uncurryN = C.uncurryN

instance Curry 4 ((a,b,c,d) -> r) (a -> b -> c -> d -> r)  where
  curryN = C.curryN
  uncurryN = C.uncurryN


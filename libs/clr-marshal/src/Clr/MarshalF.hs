{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE PolyKinds #-}

module Clr.MarshalF where

import Clr.Marshal

import Data.Kind
import GHC.TypeLits

class MarshalF (n::Nat) (from::Type) (to::Type) where
  marshalF :: from -> to

instance (Unmarshal x y) => MarshalF 0 x (IO y) where
  marshalF = unmarshal

instance ( Marshal y0 x0
         , Unmarshal xr yr
         ) => MarshalF 1 (x0 -> xr) (y0 -> IO yr) where
  marshalF f = \y0-> marshal y0 (\x0-> unmarshal $ f x0)

instance ( Marshal y0 x0
         , Marshal y1 x1
         , Unmarshal xr yr
         ) => MarshalF 2 (x0 -> x1 -> xr) (y0 -> y1 -> IO yr) where
  marshalF f = \y0-> \y1-> marshal y0 (\x0-> marshal y1 (\x1-> unmarshal $ f x0 x1))

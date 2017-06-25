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
  marshalF f y0 = marshal y0 (unmarshal . f)

instance ( Marshal y0 x0
         , Marshal y1 x1
         , Unmarshal xr yr
         ) => MarshalF 2 (x0 -> x1 -> xr) (y0 -> y1 -> IO yr) where
  marshalF f y0 y1 = marshal y0 (\x0-> marshal y1 (unmarshal . f x0))

instance ( Marshal y0 x0
         , Marshal y1 x1
         , Marshal y2 x2
         , Unmarshal xr yr
         ) => MarshalF 3 (x0 -> x1 -> x2 -> xr) (y0 -> y1 -> y2 -> IO yr) where
  marshalF f y0 y1 y2 = marshal y0 $ \x0-> marshal y1 $ \x1 -> marshal y2 (unmarshal . f x0 x1)

instance ( Marshal y0 x0
         , Marshal y1 x1
         , Marshal y2 x2
         , Marshal y3 x3
         , Unmarshal xr yr
         ) => MarshalF 4 (x0 -> x1 -> x2 -> x3 -> xr) (y0 -> y1 -> y2 -> y3 -> IO yr) where
  marshalF f y0 y1 y2 y3 = marshal y0 $ \x0-> marshal y1 $ \x1 -> marshal y2 $ \x2 -> marshal y3 (unmarshal . f x0 x1 x2)

instance ( Marshal y0 x0
         , Marshal y1 x1
         , Marshal y2 x2
         , Marshal y3 x3
         , Marshal y4 x4
         , Unmarshal xr yr
         ) => MarshalF 5 (x0 -> x1 -> x2 -> x3 -> x4 -> xr) (y0 -> y1 -> y2 -> y3 -> y4 -> IO yr) where
  marshalF f y0 y1 y2 y3 y4 = marshal y0 $ \x0-> marshal y1 $ \x1 -> marshal y2 $ \x2 -> marshal y3 $ \x3 -> marshal y4 (unmarshal . f x0 x1 x2 x3)

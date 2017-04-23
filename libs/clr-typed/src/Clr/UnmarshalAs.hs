{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}

module Clr.UnmarshalAs where

import Clr.Marshal
import Clr.Object

import Data.Int
import Data.Kind
import Data.Word
import Foreign.C

--
-- Declares how to automatically convert from the bridge type of methods result to a high level Haskell type
-- TODO: Can we do without this the end user can choose between String or Text for example?
type family   UnmarshalAs (x::Type)   :: Type
type instance UnmarshalAs (ObjectID t) = (Object t)
type instance UnmarshalAs ()           = ()
type instance UnmarshalAs Bool         = Bool
type instance UnmarshalAs Word8        = Word8
type instance UnmarshalAs Word16       = Word16
type instance UnmarshalAs Word32       = Word32
type instance UnmarshalAs Word64       = Word64
type instance UnmarshalAs Int8         = Int8
type instance UnmarshalAs Int16        = Int16
type instance UnmarshalAs Int32        = Int32
type instance UnmarshalAs Int64        = Int64
type instance UnmarshalAs Int          = Int
type instance UnmarshalAs CFloat       = Float
type instance UnmarshalAs CDouble      = Double
type instance UnmarshalAs Float        = Float
type instance UnmarshalAs Double       = Double



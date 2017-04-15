{-# LANGUAGE MultiParamTypeClasses, PolyKinds, KindSignatures, TypeFamilies, TypeSynonymInstances, FlexibleInstances, TypeOperators, TypeInType, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, UndecidableInstances #-}

module Clr.Marshal where

import Clr.Bridge
import Clr.Object

import Data.Coerce
import Data.Int
import Data.Kind
import Data.Text
import Data.Text.Foreign
import Data.Word
import Foreign.C
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable


--
-- Conversion from a high level Haskell type a a raw bridge type
--
class Marshal a b where
  marshal :: a -> (b -> IO c) -> IO c

--
-- identity instance
--
instance {-# OVERLAPPABLE #-} (a ~ b) => Marshal a b where
  marshal x f = f x

--
-- 2-tuple instance
--
instance {-# OVERLAPS #-} (Marshal a1 b1, Marshal a2 b2) => Marshal (a1, a2) (b1, b2) where
  marshal (x1,x2) f = marshal x1 $ \x1'-> marshal x2 $ \x2'-> f (x1', x2')

--
-- 3-tuple instance
--
instance {-# OVERLAPS #-} (Marshal a1 b1, Marshal a2 b2, Marshal a3 b3) => Marshal (a1, a2, a3) (b1, b2, b3) where
  marshal (x1,x2,x3) f = marshal x1 $ \x1'-> marshal x2 $ \x2'-> marshal x3 $ \x3'-> f (x1', x2', x3')

--
-- 4-tuple instance
--
instance {-# OVERLAPS #-} (Marshal a1 b1, Marshal a2 b2, Marshal a3 b3, Marshal a4 b4) => Marshal (a1, a2, a3, a4) (b1, b2, b3, b4) where
  marshal (x1,x2,x3,x4) f = marshal x1 $ \x1'-> marshal x2 $ \x2'-> marshal x3 $ \x3'-> marshal x4 $ \x4'-> f (x1', x2', x3', x4')

--
-- 5-tuple instance
--
instance {-# OVERLAPS #-} (Marshal a1 b1, Marshal a2 b2, Marshal a3 b3, Marshal a4 b4, Marshal a5 b5) => Marshal (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) where
  marshal (x1,x2,x3,x4,x5) f = marshal x1 $ \x1'-> marshal x2 $ \x2'-> marshal x3 $ \x3'-> marshal x4 $ \x4'-> marshal x5 $ \x5'-> f (x1', x2', x3', x4', x5')

--
-- Marshaling objects
--
instance {-# OVERLAPS #-} Marshal (Object t) (ObjectID t) where
  marshal (Object x) f = f x


--
-- Declares how to automatically convert from the bridge type of methods result to a high level Haskell type
-- TODO: Can we do without this the end user can choose between String or Text for example?
type family UnmarshalAs (x::Type) :: Type
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

--
-- Conversion from a raw bridge type of a methods result to a high level Haskell type
--
class Unmarshal a b where
  unmarshal :: a -> IO b

instance Unmarshal (ObjectID t) (Object t) where
  unmarshal oid = return $ Object oid


instance {-# OVERLAPPABLE #-} a ~ b => Unmarshal a b where
  unmarshal = return



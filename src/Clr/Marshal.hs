{-# LANGUAGE MultiParamTypeClasses, PolyKinds, KindSignatures, TypeFamilies, TypeSynonymInstances, FlexibleInstances, TypeOperators, TypeInType, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes, UndecidableInstances #-}

module Clr.Marshal where

import Clr.Object
import Clr.ListTuple
import Data.Kind
import Clr.Types
import Foreign.C.String
import Foreign.Ptr
import Data.Int
import Data.Type.Equality

--
-- Conversion from a high level Haskell type a a raw bridge type
--
class Marshal a b where
  marshal :: a -> (b -> IO c) -> IO c

--
-- identity instance
--
instance {-# OVERLAPPING #-} Marshal a a where
  marshal x f = f x

--
-- transitive instance
--
instance {-# OVERLAPPABLE #-} (Marshal a b, Marshal b c) => Marshal a c where
  marshal x f = marshal @a x (\x'-> marshal @b x' (\x''-> f x'') )

--
-- 2-tuple instance
--
instance (Marshal a1 b1, Marshal a2 b2) => Marshal (a1, a2) (b1, b2) where
  marshal (x1,x2) f = marshal x1 $ \x1'-> marshal x2 $ \x2'-> f (x1', x2')

--
-- 3-tuple instance
--
instance (Marshal a1 b1, Marshal a2 b2, Marshal a3 b3) => Marshal (a1, a2, a3) (b1, b2, b3) where
  marshal (x1,x2,x3) f = marshal x1 $ \x1'-> marshal x2 $ \x2'-> marshal x3 $ \x3'-> f (x1', x2', x3')

--
-- 4-tuple instance
--
instance (Marshal a1 b1, Marshal a2 b2, Marshal a3 b3, Marshal a4 b4) => Marshal (a1, a2, a3, a4) (b1, b2, b3, b4) where
  marshal (x1,x2,x3,x4) f = marshal x1 $ \x1'-> marshal x2 $ \x2'-> marshal x3 $ \x3'-> marshal x4 $ \x4'-> f (x1', x2', x3', x4')

--
-- 5-tuple instance
--
instance (Marshal a1 b1, Marshal a2 b2, Marshal a3 b3, Marshal a4 b4, Marshal a5 b5) => Marshal (a1, a2, a3, a4, a5) (b1, b2, b3, b4, b5) where
  marshal (x1,x2,x3,x4,x5) f = marshal x1 $ \x1'-> marshal x2 $ \x2'-> marshal x3 $ \x3'-> marshal x4 $ \x4'-> marshal x5 $ \x5'-> f (x1', x2', x3', x4', x5')

--
-- Boxing
--
type BoxTypes = '[ T "System.Object"    'Nothing '[]
                 , T "System.ValueType" 'Nothing '[] ]

instance {-# OVERLAPPABLE #-} (obj `Elem` BoxTypes ~ 'True, Boxable prim ) => Marshal prim (ObjectID obj) where
  marshal = box

class Boxable prim where
  box :: prim -> (ObjectID obj -> IO a) -> IO a

--
-- Marshaling objects
--
instance {-# OVERLAPPING #-} Marshal (Object t) (ObjectID t) where
  marshal (Object x) f = f x

--
-- Other Marshal instances
--
instance Marshal String CString where
  marshal = withCString

--
-- Declares how to automatically convert from the bridge type of methods result to a high level Haskell type
-- TODO: Can we do without this the end user can choose between String or Text for example?
type family UnmarshalAs (x::k) :: k' where
  UnmarshalAs   CString    = String
  UnmarshalAs (ObjectID t) = (Object t)
  UnmarshalAs     a        = a

--
-- Conversion from a raw bridge type of a methods result to a high level Haskell type
--
class Unmarshal a b where
  unmarshal :: a -> IO b

instance Unmarshal CString String where
  unmarshal cs = do
    s <- peekCString cs
    -- free cs
    return s

instance Unmarshal (ObjectID t) (Object t) where
  unmarshal oid = return $ Object oid

instance Unmarshal a a where
  unmarshal = return



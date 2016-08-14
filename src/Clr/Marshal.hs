{-# LANGUAGE MultiParamTypeClasses, PolyKinds, KindSignatures, TypeFamilies, TypeSynonymInstances, FlexibleInstances #-}

module Clr.Marshal where

import Clr.Object
import Foreign.C.String
import Data.Int

class Marshal a b where
  marshal :: a -> (b-> IO c) -> IO c

instance Marshal String CString where
  marshal = withCString

instance Marshal (Object t) ObjectID where
  marshal (Object x) f = f x

instance Marshal Int32 Int32 where
  marshal x f = f x

instance Marshal Int64 Int64 where
  marshal x f = f x

instance (Marshal a1 b1, Marshal a2 b2) => Marshal (a1, a2) (b1, b2) where
  marshal (x1,x2) f = marshal x1 (\x1'-> marshal x2 (\x2'-> f (x1', x2')))

type family UnmarshalAs (x::k) :: k'

type instance UnmarshalAs () = ()
type instance UnmarshalAs CString   = String

class Unmarshal a b where
  unmarshal :: a -> IO b

instance Unmarshal CString String where
  unmarshal cs = do
    s <- peekCString cs
    -- free cs
    return s

instance Unmarshal () () where
  unmarshal = return



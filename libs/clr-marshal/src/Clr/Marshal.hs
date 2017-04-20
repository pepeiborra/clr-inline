{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE BangPatterns #-}
module Clr.Marshal where

import Clr.Host.BStr
import Data.Coerce
import Data.Text
import Data.Text.Foreign
import Data.Word

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
-- BStr instance
--
instance Marshal Text BStr where
  marshal x f = do
    bstr <- useAsPtr x (\p-> \l-> allocBStr p l)
    !res <- f bstr
    freeBStr bstr
    return res

instance Marshal String BStr where
  marshal x f = marshal (pack x) f

--
-- Conversion from a raw bridge type of a methods result to a high level Haskell type
--
class Unmarshal a b where
  unmarshal :: a -> IO b

--
-- Identity instance
--
instance {-# OVERLAPPABLE #-} a ~ b => Unmarshal a b where
  unmarshal = return

--
-- BStr instances
--
instance Unmarshal BStr Text where
  unmarshal x = do
    let charSize = 2
    let ptrData   = coerce x              :: Ptr Word16
    let ptrLen    = plusPtr ptrData (-4)  :: Ptr Word16
    lenBytes     <- peek ptrLen
    !t <- fromPtr ptrData $ fromIntegral $ lenBytes `div` charSize
    freeBStr x
    return t

instance Unmarshal BStr String where
  unmarshal x = unmarshal x >>= return . unpack

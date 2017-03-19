{-# LANGUAGE TypeApplications, TypeInType, TypeFamilies, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

module Clr.Bindings.Marshal where

import Clr
import Clr.Bridge
import Clr.Marshal

import Clr.Host.BStr

import Data.Coerce
import Data.Text
import Data.Text.Foreign
import Data.Word
import Foreign.Ptr
import Foreign.Storable

instance Marshal Text BStr where
  marshal x f = do
    bstr <- useAsPtr x (\p-> \l-> allocBStr p l)
    f bstr

instance Marshal String BStr where
  marshal x f = marshal (pack x) f

type instance BridgeTypePrim (T "System.String" '[]) = BStr

instance Unmarshal BStr Text where
  unmarshal x = do
    let charSize = 2
    let ptrData   = coerce x              :: Ptr Word16
    let ptrLen    = plusPtr ptrData (-4)  :: Ptr Word16
    lenBytes     <- peek ptrLen
    fromPtr ptrData $ fromIntegral $ lenBytes `div` charSize

instance Unmarshal BStr String where
  unmarshal x = unmarshal x >>= return . unpack

type instance UnmarshalAs BStr = String


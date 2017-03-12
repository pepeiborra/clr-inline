{-# LANGUAGE TypeApplications, TypeInType, TypeFamilies, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

module Clr.Bindings.Marshal where

import Clr
import Clr.Bridge
import Clr.Marshal

import Clr.Host.BStr

import Control.Exception(finally)
import Data.Text
import Data.Text.Foreign

instance Marshal Text BStr where
  marshal x f = do
    bstr <- useAsPtr x (\p-> \l-> allocBStr p l)
    finally (f bstr) (freeBStr bstr)

instance Marshal String BStr where
  marshal x f = marshal (pack x) f

type instance BridgeTypePrim (T "System.String" '[]) = BStr


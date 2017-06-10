{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, TypeInType, TypeFamilies, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

module Clr.Bindings.BStr where

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


type instance BridgeTypePrim (T "System.String" '[]) = BStr


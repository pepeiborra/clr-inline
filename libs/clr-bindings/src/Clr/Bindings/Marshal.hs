{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeApplications, TypeInType, TypeFamilies, TypeSynonymInstances, MultiParamTypeClasses, FlexibleInstances #-}

module Clr.Bindings.Marshal where

import Clr
import Clr.Bridge
import Clr.Marshal
import Clr.Unmarshal

import Clr.Host.BStr

import Data.Coerce
import Data.Text
import Data.Text.Foreign
import Data.Word
import Foreign.Ptr
import Foreign.Storable


type instance BridgeTypePrim (T "System.String" '[]) = BStr

--
-- NB: This next line specifies that all methods
-- returning a System.String, get the result
-- converted to a Text. This needs a bit of work,
-- but polymorphism in the return type is going
-- to make compilation a lot harder. TODO.
--
type instance UnmarshalAs BStr = Text


{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
{-# LANGUAGE KindSignatures, GADTs, TypeInType #-}

module Clr.Object where

import Clr.Marshal
import Clr.Types
import Clr.TypeString
import Data.Kind
import Data.Int
import Foreign.ForeignPtr

--
-- An object is just its unique identifer + information of its type
--
data Object (typ::Type) where
  Object :: (TString typ) => ForeignPtr Int -> Object typ


type instance HaskToClr (Object t) = t



module Clr.Host.BStr.Type where

import Data.Word
import Foreign.Ptr

newtype BStr = BStr (Ptr Word16) deriving (Show)


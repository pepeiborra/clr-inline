module Clr.Host.BStr.Mono where

import Clr.Host.BStr.Type
import Data.Word
import Foreign.C
import Foreign.Ptr

foreign import ccall mono_ptr_to_bstr :: Ptr Word16 -> CUInt -> IO BStr
foreign import ccall mono_free_bstr :: BStr -> IO ()


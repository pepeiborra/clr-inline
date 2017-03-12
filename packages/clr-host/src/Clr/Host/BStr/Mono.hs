module Clr.Host.BStr.Mono where

import Clr.Host.BStr.Type
import Data.Coerce
import Data.Word
import Foreign.C
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable

--
-- This function is not exported so a reimplemention of it is below
-- foreign import ccall mono_ptr_to_bstr :: Ptr Word16 -> CUInt -> IO BStr
--

type GSize = CULong

foreign import ccall g_malloc :: GSize -> IO (Ptr a)

mono_ptr_to_bstr_hask :: Ptr Word16 -> CUInt -> IO BStr
mono_ptr_to_bstr_hask p l = do
  let charSize = 2
  ptrLen <- g_malloc (fromIntegral ((l + 1) * charSize + 4))  :: IO (Ptr Word32)
  poke ptrLen (fromIntegral (l * charSize))
  let ptrData = plusPtr ptrLen 4                              :: Ptr Word16
  copyBytes ptrData p (fromIntegral (l * charSize))
  let ptrTerm = plusPtr ptrData (fromIntegral (l * charSize)) :: Ptr Word16
  poke ptrTerm 0
  return $ coerce ptrData

foreign import ccall mono_free_bstr :: BStr -> IO ()


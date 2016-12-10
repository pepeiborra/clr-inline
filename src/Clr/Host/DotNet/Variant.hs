{-# LANGUAGE CPP #-}

module Clr.Host.DotNet.Variant where

import Data.Word
import Foreign.Ptr
import Foreign.Storable

data Variant = Variant VarType Word64 deriving (Show, Eq)
type VarType = Word16

varType_Empty, varType_UI1 :: VarType
varType_Empty = 0
varType_UI1   = 17

emptyVariant = Variant varType_Empty 0

instance Storable Variant where
#if x86_64_HOST_ARCH
  sizeOf    _ = 24
#else
  sizeOf    _ = 16
#endif
  alignment _ = 8

  peek ptr = do
    a  <- peek $ plusPtr ptr 0
    b  <- peek $ plusPtr ptr 8
    return $ Variant a b

  poke ptr (Variant a b) = do
    poke (plusPtr ptr 0) a
    poke (plusPtr ptr 2) (0 :: Word16)
    poke (plusPtr ptr 4) (0 :: Word16)
    poke (plusPtr ptr 6) (0 :: Word16)
    poke (plusPtr ptr 8) b


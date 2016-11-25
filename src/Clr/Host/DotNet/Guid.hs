module Clr.Host.DotNet.Guid where

data Guid = Guid Word32 Word16 Word16 Word8 Word8 Word8 Word8 Word8 Word8 Word8 Word8
  deriving (Show, Eq)

instance Storable Guid where
  sizeOf    _ = 16
  alignment _ = 4

  peek guidPtr = do
    a  <- peek $ plusPtr guidPtr 0
    b  <- peek $ plusPtr guidPtr 4
    c  <- peek $ plusPtr guidPtr 6
    d0 <- peek $ plusPtr guidPtr 8
    d1 <- peek $ plusPtr guidPtr 9
    d2 <- peek $ plusPtr guidPtr 10
    d3 <- peek $ plusPtr guidPtr 11
    d4 <- peek $ plusPtr guidPtr 12
    d5 <- peek $ plusPtr guidPtr 13
    d6 <- peek $ plusPtr guidPtr 14
    d7 <- peek $ plusPtr guidPtr 15
    return $ Guid a b c d0 d1 d2 d3 d4 d5 d6 d7

  poke guidPtr (Guid a b c d0 d1 d2 d3 d4 d5 d6 d7) = do
    poke (plusPtr guidPtr 0)  a
    poke (plusPtr guidPtr 4)  b
    poke (plusPtr guidPtr 6)  c
    poke (plusPtr guidPtr 8)  d0
    poke (plusPtr guidPtr 9)  d1
    poke (plusPtr guidPtr 10) d2
    poke (plusPtr guidPtr 11) d3
    poke (plusPtr guidPtr 12) d4
    poke (plusPtr guidPtr 13) d5
    poke (plusPtr guidPtr 14) d6
    poke (plusPtr guidPtr 15) d7

type CLSID    = Guid
type IID      = Guid


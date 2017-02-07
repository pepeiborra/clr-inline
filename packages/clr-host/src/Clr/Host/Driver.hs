{-# LANGUAGE TemplateHaskell #-}

module Clr.Host.Driver (driverData) where

import qualified Data.ByteString.Char8 as B
import Data.FileEmbed

{-# NOINLINE driverData #-}
driverData :: B.ByteString
driverData = $(makeRelativeToProject "src/Driver.dll" >>= embedFile)


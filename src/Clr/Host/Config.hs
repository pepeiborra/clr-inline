module Clr.Host.Config where

data ClrHostType = ClrHostMono | ClrHostDotNet

data ClrHostConfig = ClrHostConfig ClrHostType

getClrHostConfig :: IO ClrHostConfig
getClrHostConfig = return $ ClrHostConfig ClrHostMono


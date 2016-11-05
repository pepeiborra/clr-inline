module Clr.Host where

import Clr.Host.Config
import Clr.Host.Mono
import Clr.Host.DotNet

startClr :: IO ()
startClr = do
  putStrLn "startClr"
  hostConfig <- getClrHostConfig
  case hostConfig of
    ClrHostConfig ClrHostMono   -> startHostMono
    ClrHostConfig ClrHostDotNet -> startHostDotNet

stopClr :: IO ()
stopClr = putStrLn "stopClr"


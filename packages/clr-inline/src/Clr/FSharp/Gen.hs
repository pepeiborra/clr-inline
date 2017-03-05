module Clr.FSharp.Gen where

import Data.Typeable
import Language.Haskell.TH

data FSharpUnit = FSharpUnit
  { name, body :: String
  , wrapperName :: Name
  , args :: [String]
  } deriving (Show, Typeable)

data FSharpModule = FSharpModule
  { moduleName :: String
  , moduleNamespace  :: String
  , moduleUnits :: [FSharpUnit]}

genCode :: FSharpModule -> String
genCode _ = ""

compile :: FSharpModule -> IO String
compile _ = return ""

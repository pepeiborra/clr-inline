{-# LANGUAGE RecordWildCards #-}
module Clr.FSharp.Gen (FSharpUnit(..), FSharpModule(..), compile) where

import           Clr.Inline.Utils
import qualified Data.ByteString     as BS
import           Data.List
import           Data.Typeable
import           Language.Haskell.TH
import           System.Directory
import           System.FilePath     ((<.>), (</>))
import           System.IO.Temp
import           System.Process
import           Text.Printf

data FSharpUnit = FSharpUnit
  { name, body  :: String
  , args        :: [String]
  } deriving (Show, Typeable)

data FSharpModule = FSharpModule
  { moduleName      :: String
  , moduleNamespace :: String
  , moduleUnits     :: [FSharpUnit]}

-- The name of the F# compiler in Mono.
-- Probably platform specific
fsharp = "fsharpc"

genCode :: FSharpModule -> String
genCode FSharpModule{..} =
  unlines $
    [printf "namespace %s" moduleNamespace
    ,"open System" -- TODO imports
    ,printf "module %s = " moduleName
    ] ++ concat
    [ printf "    let %s (%s) = " name (intercalate ", " args) :
      [ printf "        %s" l | l <- lines body ]
    | FSharpUnit{..} <- moduleUnits
    ]

compile :: FSharpModule -> IO ClrBytecode
compile m@FSharpModule {..} = do
    temp <- getTemporaryDirectory
    dir <- createTempDirectory temp "inline-clr"
    let src = dir </> moduleName <.> ".fs"
        tgt = dir </> moduleName <.> ".dll"
    writeFile src (genCode m)
    putStrLn $ "Generated " ++ tgt
    callProcess fsharp ["--target:library", "--out:"++tgt, src]
    bcode <- BS.readFile tgt
    return $ ClrBytecode bcode

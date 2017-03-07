{-# LANGUAGE RecordWildCards #-}
module Clr.FSharp.Gen (compile) where

import           Clr.Inline.Utils
import           Clr.Inline.Types
import qualified Data.ByteString     as BS
import           Data.List
import           Data.Typeable
import           Language.Haskell.TH
import           System.Directory
import           System.FilePath     ((<.>), (</>))
import           System.IO.Temp
import           System.Process
import           Text.Printf

-- The name of the F# compiler in Mono.
-- TODO support both "fsc" and "fsharpc"
fsharp = "fsharpc"

data FSharp

genCode :: ClrInlinedGroup FSharp -> String
genCode ClrInlinedGroup {..} =
  unlines $
  [ printf "namespace %s" modNamespace
  , "open System" -- TODO imports
  , printf "module %s = " modName
  ] ++
  concat
    [ printf
      "    let %s (%s) = "
      name
      (intercalate ", " $ zipWith (printf "%s:$s") args argTypes) :
    [printf "        %s" l | l <- lines body]
    | ClrInlinedUnit {..} <- units
    ]

compile :: ClrInlinedGroup FSharp -> IO ClrBytecode
compile m@ClrInlinedGroup {..} = do
    temp <- getTemporaryDirectory
    dir <- createTempDirectory temp "inline-fsharp"
    let src = dir </> modName <.> ".fs"
        tgt = dir </> modName <.> ".dll"
    writeFile src (genCode m)
    putStrLn $ "Generated " ++ tgt
    callProcess fsharp ["--target:library", "--out:"++tgt, src]
    bcode <- BS.readFile tgt
    return $ ClrBytecode bcode

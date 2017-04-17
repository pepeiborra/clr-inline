module Clr.ImportGen.Definition where

import qualified Data.Text as T
import Data.List(intercalate)

data Ref    = Ref T.Text
data Import = Import T.Text [T.Text]

instance Show Ref where
  show (Ref t) = T.unpack t
instance Show Import where
  show (Import ns [])   = T.unpack ns
  show (Import ns typs) = T.unpack ns ++ "(" ++ intercalate "," (map T.unpack typs) ++ ")"

data RefImportDef
  = RefImportDef
  { getRefs :: [Ref]
  , getImps :: [Import] }

instance Show RefImportDef where
  show (RefImportDef refs imps) = "ImportDef\n\trefs="
                               ++ show refs
                               ++ "\n\tNS="
                               ++ show imps

emptyDef :: RefImportDef
emptyDef = RefImportDef [] []


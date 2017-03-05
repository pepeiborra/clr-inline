{-# LANGUAGE LambdaCase #-}
module Clr.Inline.Utils where

import Data.Char
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Text.Printf

mangleModule :: Module -> String
mangleModule (Module (PkgName pkg) (ModName m)) =
  printf "Inline__%s_%s" (filter isAlphaNum pkg) (map (\case '.' -> '_' ; x -> x) m)

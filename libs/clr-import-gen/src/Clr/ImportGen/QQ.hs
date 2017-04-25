{-# LANGUAGE TemplateHaskell #-}

module Clr.ImportGen.QQ where

import Clr.ImportGen.Definition
import Clr.ImportGen.Parser
import Clr.ImportGen.Processor

import Language.Haskell.TH.Quote
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Syntax

import qualified Data.Text as T

clrImportGen = QuasiQuoter { quoteExp  = undefined
                           , quotePat  = undefined
                           , quoteType = undefined
                           , quoteDec  = importGenDec }

importGenDec :: String -> Q [Dec]
importGenDec s = do
  let txt = T.pack s
  case parseImportDefs txt of
    Left errorStr -> error errorStr
    Right def     -> importDefToQDec def


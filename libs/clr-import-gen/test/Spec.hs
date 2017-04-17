{-# LANGUAGE OverloadedStrings #-}

import Clr.ImportGen.Definition
import Clr.ImportGen.Parser

import Data.Attoparsec.Text
import qualified Data.Text as T

testDefStr :: T.Text
testDefStr = "ref foo\nimport bar\nimport car\nimport One.Two.Three"

main :: IO ()
main = do
  defs <- return $ parseImportDefs testDefStr
  putStrLn ""
  print defs
  return ()

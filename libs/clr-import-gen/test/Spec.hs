{-# LANGUAGE OverloadedStrings #-}

import Clr.ImportGen.Definition
import Clr.ImportGen.Parser

import Data.Attoparsec.Text
import qualified Data.Text as T

import Test.Hspec

testDefStr :: T.Text
testDefStr = T.pack $
  "ref foo"
  ++ "\n" ++
  "import bar"
  ++ "\n" ++
  "import One.Two.Three(something)   "
  ++ "\n" ++
  "import NS(     thisThing, thatThing  )"
  ++ "\n" ++
  "ref     Somecomplicatedref, version=1.2.3, culture=neutral   "

main :: IO ()
main = do
  let defs = parseImportDefs testDefStr
  defs `shouldBe` Right (
    RefImportDef
    [ Ref "foo"
    , Ref "Somecomplicatedref, version=1.2.3, culture=neutral" ]
    [ Import "bar" []
    , Import "One.Two.Three" ["something"]
    , Import "NS" ["thisThing", "thatThing"] ] )
  return ()

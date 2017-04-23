{-# LANGUAGE OverloadedStrings, TypeInType #-}

import Test.Hspec

import Clr

import Clr.Host

import Clr.ImportGen.Definition
import Clr.ImportGen.Parser
import Clr.ImportGen.Processor
import Clr.ImportGen.Reflection

import Data.Attoparsec.Text
import qualified Data.Text as T
import Pipes
import Pipes.Prelude
import Pipes.Prelude.Text

import Language.Haskell.TH(runQ, Type, ppr)

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

printGenArgs :: Object T_Type -> IO T.Text
printGenArgs typ = do
  genArgs <- toListM $ typeGetGenericArguments typ
  names   <- Prelude.mapM typeName genArgs
  let gtNames = Prelude.map (\n-> "gt_" `T.append` n) names
  return $ "'[" `T.append` (T.intercalate ", " gtNames) `T.append` "]"

printType :: Object T_Type -> IO T.Text
printType typ = do
  name    <- typeFullName typ
  nm      <- typeFullNm typ
  genArgs <- printGenArgs typ
  rep <- runQ $ typeGetHaskellRepresentation typ :: IO Type
  return $ name `T.append` " -> " `T.append` (T.pack $ Prelude.show $ ppr rep)

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
  --startClr
  --runEffect $ knownTypes >-> Pipes.Prelude.filterM typeIsSupported >-> Pipes.Prelude.mapM printType >-> Pipes.Prelude.Text.stdoutLn
  return ()

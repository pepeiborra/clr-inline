
import Clr.ImportGen.Definition
import Clr.ImportGen.Parser

import Data.Attoparsec.Text
import qualified Data.Text as T

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
  defs <- return $ parseImportDefs testDefStr
  putStrLn ""
  print defs
  return ()

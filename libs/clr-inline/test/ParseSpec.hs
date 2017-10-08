{-# LANGUAGE PatternSynonyms #-}
module ParseSpec(spec) where

import Clr.Inline.Types
import Clr.Inline.Types.Parse
import Control.Lens
import Test.Hspec

spec :: Spec
spec = parseSpec

shouldParseTo program result = do
  let p = view tokenized program
  p `shouldBe` result

shouldRoundtripAndParseTo program result = do
  let p = view tokenized program
  p `shouldBe` result
  review tokenized p `shouldBe` program


parseSpec :: Spec
parseSpec = do
  it "$foo"                      $ "$foo"                   `shouldRoundtripAndParseTo` [Antiquote False "foo" Nothing]
  it "$foo:int"                  $ "$foo:int"               `shouldRoundtripAndParseTo` [Antiquote False "foo" (Just (Con "int"))]
  it "$foo:unit"                 $ "$foo:unit"              `shouldRoundtripAndParseTo` [Antiquote False "foo" (Just Unit)]
  it "$(foo:unit)"               $ "$(foo:unit)"            `shouldRoundtripAndParseTo` [Antiquote True  "foo" (Just Unit)]
  it "($foo:unit)"               $ "($foo:unit)"            `shouldRoundtripAndParseTo` [Other "(", Antiquote False "foo" (Just Unit), Other ")"]
  it "$$foo"                     $ "$$foo"                  `shouldParseTo`             [Other "$foo"]
  it "$foo:int[]"                $ "$foo:int[]"             `shouldRoundtripAndParseTo` [Antiquote False "foo" (Just $ Array 1 $ Con "int")]
  it "$foo:int[,]"               $ "$foo:int[,]"            `shouldRoundtripAndParseTo` [Antiquote False "foo" (Just $ Array 2 $ Con "int")]
  it "$foo:int[][]"              $ "$foo:int[][]"           `shouldRoundtripAndParseTo` [Antiquote False "foo" (Just $ Array 1 $ Array 1 $ Con "int")]
-- it "\"$foo\""                 $ "\"$foo\""               `shouldRoundtripAndParseTo` [Other "\"$foo\""] TODO
  it "1 $ 2"                     $ "1 $ 2"                  `shouldRoundtripAndParseTo` [Other "1 $ 2"]
  it "1 $ 2"                     $ "1 $ 2"                  `shouldRoundtripAndParseTo` [Other "1 $ 2"]
  it "a->b"                      $ "a->b"                   `shouldRoundtripAndParseTo` [Other "a->b"]
  it "$a->b"                     $ "$a->b"                  `shouldRoundtripAndParseTo` [Antiquote False "a" Nothing, Other "->b"]
  it "$foo:a->b"                 $ "$foo:a->b"              `shouldRoundtripAndParseTo` [Antiquote False "foo" (Just (Fun [Con "a"] (Con "b")))]
  it "$foo:unit->b"              $ "$foo:unit->b"           `shouldRoundtripAndParseTo` [Antiquote False "foo" (Just (Fun [Unit] (Con "b")))]
  it "$foo:unit->unit"           $ "$foo:unit->unit"        `shouldRoundtripAndParseTo` [Antiquote False "foo" (Just (Fun [Unit] Unit))]
  it "$foo:IEnumerable<int>"     $ "$foo:IEnumerable<int>"  `shouldRoundtripAndParseTo` [Antiquote False "foo" (Just $ TyCon "IEnumerable" [Con "int"])]
  it "$foo:Map<int,bool>"        $ "$foo:Map<int,bool>"     `shouldRoundtripAndParseTo` [Antiquote False "foo" (Just $ TyCon "Map" [Con "int", Con "bool"])]
  it "$foo:IEnumerable<int>[]"   $ "$foo:IEnumerable<int>[]"`shouldRoundtripAndParseTo` [Antiquote False "foo" (Just $ Array 1 $ TyCon "IEnumerable" [Con "int"])]

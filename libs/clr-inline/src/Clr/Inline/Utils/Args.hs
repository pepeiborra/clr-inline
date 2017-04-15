module Clr.Inline.Utils.Args where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map

data Token =
    Other String
  | Antiquote String (Maybe String)
  deriving Show

tokenized :: Iso' String [Token]
tokenized = iso (tokenize (Other [])) untokenize
  where
    tokenize :: Token -> String -> [Token]
    -- Tokenizing inside clr code
    tokenize (Other acc) [] = [Other (reverse acc)]
    -- Start an antiquote
    tokenize (Other acc) ('$':rest) = Other (reverse acc) : tokenize (Antiquote "" Nothing) rest
    -- Escape F# array notation
    tokenize (Other ('~':'|':acc)) (']':rest) = tokenize (Other (']':'|':acc)) rest
    tokenize (Other ('~':'[':acc)) ('|':rest) = tokenize (Other ('|':'[':acc)) rest
    tokenize (Other acc) (c  :rest) = tokenize (Other (c:acc)) rest
    -- Tokenizing inside an antiquote
    tokenize (Antiquote s t) [] = [Antiquote (reverse s) (reverse <$> t)]
    tokenize (Antiquote s t) (c : rest) | isBreak c = Antiquote (reverse s) (reverse <$> t) : tokenize (Other [c]) rest
    tokenize (Antiquote s Nothing)  (':':rest) = tokenize (Antiquote s (Just "")) rest
    tokenize (Antiquote s Nothing)  (c  :rest) = tokenize (Antiquote (c:s) Nothing) rest
    tokenize (Antiquote s (Just t))   (c  :rest) = tokenize (Antiquote s (Just (c:t))) rest

    untokenize :: [Token] -> String
    untokenize [] = []
    untokenize (Other s: rest) = s ++ untokenize rest
    untokenize (Antiquote v Nothing  : rest) = '$' : v ++ untokenize rest
    untokenize (Antiquote v (Just t) : rest) = '$' : v ++ ':' : t ++ untokenize rest

    isBreak c = c == ' ' ||  c == ')'

-- | Looks for antiquotes of the form $foo in the given string
--   Returns the antiquotes found, and a new string with the
--   antiquotes transformed
extractArgs :: (String -> String) -> String -> (Map String String, String)
extractArgs transf = mapAccumROf (tokenized.traversed) f mempty
  where
    f acc (Other s) = (acc, Other s)
    f acc (Antiquote v (Just t)) = (Map.insert v t acc, Other (transf v))
    f acc (Antiquote v Nothing)
      | Just _ <- acc ^? at v = (acc, Other (transf v))
      | otherwise = error $ "The first occurrence of an antiquote must include a type ann. (" ++ v ++ ")"

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Clr.Inline.Utils.Args where

import Control.Lens
import Data.Map (Map)
import qualified Data.Map as Map

data Token =
    Other String
  | Antiquote String String
  deriving Show

tokenized :: Iso' String [Token]
tokenized = iso (tokenize (Other [])) (untokenize)
  where
    tokenize :: Token -> String -> [Token]
    tokenize (Other acc) [] = [Other (reverse acc)]
    tokenize (Antiquote s t) [] = [Antiquote (reverse s) (reverse t)]
    tokenize (Other acc) ('$':rest) = Other (reverse acc) : tokenize (Antiquote "" "") rest
    tokenize (Other acc) (c  :rest) = tokenize (Other (c:acc)) rest
    tokenize (Antiquote s t) (c : rest) | isBreak c = Antiquote (reverse s) (reverse t) : tokenize (Other [c]) rest
    tokenize (Antiquote s [])  (':':rest) = tokenize (Antiquote s ":") rest
    tokenize (Antiquote s [])  (c  :rest) = tokenize (Antiquote (c:s) [ ]) rest
    tokenize (Antiquote s ":") (c  :rest) = tokenize (Antiquote s     [c]) rest
    tokenize (Antiquote s t)   (c  :rest) = tokenize (Antiquote s (c:t)) rest

    untokenize :: [Token] -> String
    untokenize [] = []
    untokenize (Other s: rest) = s ++ untokenize rest
    untokenize (Antiquote v t : rest) = '$' : v ++ ':' : t ++ untokenize rest

    isBreak c = c == ' ' ||  c == ')' || c == '.'

-- | Looks for antiquotes of the form $foo in the given string
--   Returns the antiquotes found, and a new string with the
--   antiquotes transformed
extractArgs :: (String -> String) -> String -> (Map String String, String)
extractArgs transf = mapAccumROf (tokenized.traversed) f mempty
  where
    f acc (Other s) = (acc, Other s)
    f acc (Antiquote v t) = (Map.insert v t acc, Other (transf v))

module Clr.Inline.Utils.Parse where

import Control.Lens
import Data.Char
import Data.List.Extra
import Data.Map (Map)
import qualified Data.Map as Map

data Token =
    Other String
  | Antiquote String (Maybe String)
  deriving Show

-- TODO tokenizing quoted strings
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
    tokenize (Antiquote s (Just t)) (c  :rest) = tokenize (Antiquote s (Just (c:t))) rest

    untokenize :: [Token] -> String
    untokenize [] = []
    untokenize (Other s: rest) = s ++ untokenize rest
    untokenize (Antiquote v Nothing  : rest) = '$' : v ++ untokenize rest
    untokenize (Antiquote v (Just t) : rest) = '$' : v ++ ':' : t ++ untokenize rest

    isBreak c = isSpace c ||  c == ')'

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

-- | Fix different systems silly line ending conventions
--   https://ghc.haskell.org/trac/ghc/ticket/11215
normaliseLineEndings :: String -> String
normaliseLineEndings []            = []
normaliseLineEndings ('\r':'\n':s) = '\n' : normaliseLineEndings s -- windows
normaliseLineEndings ('\r':s)      = '\n' : normaliseLineEndings s -- old OS X
normaliseLineEndings (  c :s)      =   c  : normaliseLineEndings s

initAndLast :: String -> Maybe (String, Char)
initAndLast = loopInitAndLast id where
  loopInitAndLast _   [ ]    = Nothing
  loopInitAndLast acc [x]    = Just (acc "", x)
  loopInitAndLast acc (x:xx) = loopInitAndLast (acc . (x:)) xx

-- | Parses expressions of the form "ty{e}" and returns (ty, e)
parseBody :: String -> (String, String)
parseBody e =
  case span ('{' /=) (trim e) of
    (typeString, exp') ->
      case initAndLast (drop 1 exp') of
        Just (exp,'}') -> (trim typeString, exp)
        _ -> ("void", e)

data ParseResult = ParseResult
  { body, returnType :: String
  , args :: Map String String
  }

parse :: (String -> String) -> String -> ParseResult
parse transf inline = ParseResult b ret args where
  (ret, inline') = parseBody inline
  (args, b) = extractArgs transf inline'

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE LambdaCase #-}
module Clr.Inline.Types.Parse where

import Clr.Inline.Types
import Control.Lens
import qualified Data.CaseInsensitive as CI
import Data.Char
import Data.List.Extra
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map
import Text.Parsec.Combinator
import Text.Parsec.Pos
import Text.Parsec.Prim
import Prelude hiding (getChar)

type Parser a = Parsec [Token] () a

data Token = Char Char | Dollar | Arrow deriving Eq

tokenize :: String -> [Token]
tokenize ('[':'~':'|': rest) = Char '[' : Char '|' : tokenize rest
tokenize ('|':'~':']': rest) = Char '|' : Char ']' : tokenize rest
tokenize ('$':'$':xx) = Char '$' : tokenize xx
tokenize ('-':'>':xx) = Arrow : tokenize xx
tokenize ('$':xx) = Dollar : tokenize xx
tokenize (x:xx) = Char x : tokenize xx
tokenize [] = []

isChar :: Token -> Bool
isChar = isJust . getChar

getChar :: Token -> Maybe Char
getChar (Char c) = Just c
getChar _ = Nothing

isDollar :: Token -> Bool
isDollar = not . isChar

tokenToString :: Token -> String
tokenToString (Char c) = [c]
tokenToString Dollar   = "$"
tokenToString Arrow = "->"

instance Show Token where
  show = tokenToString

data Section =
    Other String
  | Antiquote {parenthised :: Bool, name:: !String, typ :: !(Maybe ClrType)}
  deriving (Eq, Show)

normalizeProgram :: [Section] -> [Section]
normalizeProgram (Other a : Other b : rest) = Other(a++b) : normalizeProgram rest
normalizeProgram (x:xx) = x : normalizeProgram xx
normalizeProgram [] = []

satisfy :: (Token -> Maybe a) -> Parser a
satisfy = tokenPrim show (\pos t _cs -> updatePosString pos (tokenToString t))

dollar :: Parser ()
dollar = satisfy (\case Dollar -> Just () ; _ -> Nothing)
arrow :: Parser String
arrow  = const "->" <$> satisfy (\case Arrow  -> Just () ; _ -> Nothing)


char :: Char -> Parser Char
char c = satisfyChar (== c)

string :: String -> Parser String
string = mapM char

satisfyChar :: (Char -> Bool) -> Parser Char
satisfyChar f = satisfy (\case Char c | f c -> Just c ; _ -> Nothing)

topP :: Parser [Section]
topP = (normalizeProgram <$> many sectionP) <* eof

sectionP :: Parser Section
sectionP = (dollar *> (antiquoteP False <|> otherP "$")) <|>
           otherP ""

otherP :: String -> Parser Section
otherP prefix = Other . (prefix ++) . concat <$> many1 (((:[]) <$> satisfy getChar) <|> arrow)

antiquoteP :: Bool -> Parser Section
antiquoteP parenthised = parens (antiquoteP True) <|>
             (Antiquote parenthised <$> identP <*> option Nothing (Just <$> (char ':' *> typP)))

parens :: Parser a -> Parser a
parens = between (char '(') (char ')')

identP :: Parser String
identP = (:) <$> satisfyChar isAlpha <*> many(satisfyChar isIdent)
  where
    isIdent x = isAlphaNum x || x == '_'

conP :: Parser String
conP   = (:) <$> satisfyChar isTypeIdent <*> many(satisfyChar isTypeIdent)
  where
    isTypeIdent x = isAlphaNum x || x `elem` ("._-*"::String)

tyconP,tyconFunP,typP :: Parser ClrType
tyconP =
  TyCon <$> conP <*>
    (    between (char '<') (char '>') (typP `sepBy` char ',')
     <|> pure [])

tyconFunP = rebuild <$> tyconP <*> optionMaybe (arrow *> tyconFunP)
  where
    rebuild :: ClrType -> Maybe ClrType -> ClrType
    rebuild con Nothing = con
    rebuild con (Just (Fun args' res)) = Fun (args' ++ [con]) res
    rebuild con (Just res) = Fun [con] res

typP = flip ($) <$> tyconFunP <*> (foldr (.) id <$> many arrayP)
  where
    arrayP = Array . succ . length <$> between (char '[') (char ']') (many (char ','))

pattern CI :: CI.FoldCase s => CI.CI s -> s
pattern CI s <- (CI.mk -> s)

-- TODO tokenizing quoted strings
tokenized :: Iso' String [Section]
tokenized = iso parse untokenize
  where
    parse x = case runParser topP () "" (tokenize x) of
                Right res -> res
                Left e -> error $ show e

    untokenize :: [Section] -> String
    untokenize [] = []
    untokenize (Other s: rest) = s ++ untokenize rest
    untokenize (Antiquote True  v Nothing  : rest) = '$':'(':v ++ ')':untokenize rest
    untokenize (Antiquote True  v (Just t) : rest) = '$':'(':v ++ ':' : renderClrType t ++ ')':untokenize rest
    untokenize (Antiquote False v Nothing  : rest) = '$' : v ++ untokenize rest
    untokenize (Antiquote False v (Just t) : rest) = '$' : v ++ ':' : renderClrType t ++ untokenize rest

-- | Looks for antiquotes of the form $foo in the given string
--   Returns the antiquotes found, and a new string with the
--   antiquotes transformed
extractArgs :: (String -> String) -> String -> (Map String ClrType, String)
extractArgs transf = mapAccumROf (tokenized.traversed) f mempty
  where
    f acc (Other s) = (acc, Other s)
    f acc (Antiquote _ v (Just t)) = (Map.insert v t acc, Other (transf v))
    f acc (Antiquote _ v Nothing )
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
  , args :: Map String ClrType
  }

parse :: (String -> String) -> String -> ParseResult
parse transf inline = ParseResult b ret args where
  (ret, inline') = parseBody inline
  (args, b) = extractArgs transf inline'

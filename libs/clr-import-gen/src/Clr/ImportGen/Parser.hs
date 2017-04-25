{-# LANGUAGE OverloadedStrings #-}

module Clr.ImportGen.Parser where

import Clr.ImportGen.Definition

import Control.Applicative

import Data.Attoparsec.Text
import qualified Data.Text as T

--
-- Each line starts a particular keyword
--
s_ref, s_import :: T.Text
s_ref    = "ref"
s_import = "import"

--
-- Each line represents an assembly reference or a namespace import with an optional subset of just certains from the specified namespace
--
data Line = LineImport T.Text [T.Text]
          | LineRef    T.Text

--
-- updateDef newLine def evaluates to def with newLine included
--
updateDef :: Line -> RefImportDef -> RefImportDef
updateDef l def = case l of
  (LineImport ns typs) -> RefImportDef (getRefs def) (Import ns typs : getImps def)
  (LineRef ref)        -> RefImportDef (Ref ref : getRefs def) (getImps def)

linesToImportDef :: [Line] -> RefImportDef
linesToImportDef = foldr updateDef emptyDef

lineParser :: Parser Line
lineParser = lineImportParser <|> lineRefParser

isNameSpaceChar :: Char -> Bool
isNameSpaceChar = inClass "a-zA-Z.-"

isTypeNameChar :: Char -> Bool
isTypeNameChar = inClass "a-zA-Z-"

--
-- isSpace' is like isSpace except it evaluates to false for end of line (carriage returns and line feeds)
--
isSpace' :: Char -> Bool
isSpace' c = c `elem` (" \t\f\v" :: String)

skipSpace' :: Parser ()
skipSpace' = skipWhile isSpace'

--
-- inSpace p runs the parser p, skipping over space before and after
--
inSpace :: Parser a -> Parser a
inSpace p = do
  skipSpace'
  ret <- p
  skipSpace'
  return ret

parseTypeName :: Parser T.Text
parseTypeName = inSpace $ takeWhile1 isTypeNameChar

parseImportParenTypes :: Parser [T.Text]
parseImportParenTypes = do
  inSpace $ string "("
  typs <- inSpace $ parseTypeName `sepBy1` (char ',')
  inSpace $ string ")"
  return typs

lineImportParser :: Parser Line
lineImportParser = do
  inSpace $ string s_import
  ns <- inSpace $ takeWhile1 isNameSpaceChar
  typs <- inSpace $ option [] parseImportParenTypes
  endOfLine <|> endOfInput
  return $ LineImport ns typs

lineRefParser :: Parser Line
lineRefParser = do
  inSpace $ string s_ref
  ref <- takeTill isEndOfLine
  endOfLine <|> endOfInput
  return $ LineRef $ T.strip ref

importDefParser :: Parser RefImportDef
importDefParser = do
  ls <- many lineParser
  endOfInput
  return $ linesToImportDef ls

parseImportDefs :: T.Text -> Either String RefImportDef
parseImportDefs = parseOnly importDefParser



{-# LANGUAGE OverloadedStrings #-}

module Clr.ImportGen.Parser where

import Clr.ImportGen.Definition

import Control.Applicative

import Data.Attoparsec.Text
import qualified Data.Text as T

s_ref, s_import :: T.Text
s_ref    = "ref"
s_import = "import"

data Line = LineImport T.Text [T.Text]
          | LineRef    T.Text

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

isSpace' :: Char -> Bool
isSpace' c = c `elem` (" \t\f\v" :: String)

skipSpace' :: Parser ()
skipSpace' = skipWhile isSpace'

parseTypeName :: Parser T.Text
parseTypeName = do
  skipSpace'
  typeName <- takeWhile1 isTypeNameChar
  skipSpace'
  return typeName

parseImportParenTypes :: Parser [T.Text]
parseImportParenTypes = do
  skipSpace'
  string "("
  typs <- parseTypeName `sepBy1` (char ',')
  skipSpace'
  string ")"
  skipSpace'
  return typs

lineImportParser :: Parser Line
lineImportParser = do
  skipSpace'
  string s_import
  skipSpace'
  ns <- takeWhile1 isNameSpaceChar
  skipSpace'
  typs <- option [] parseImportParenTypes
  endOfLine <|> endOfInput
  return $ LineImport ns typs

lineRefParser :: Parser Line
lineRefParser = do
  skipSpace'
  string s_ref
  skipSpace'
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



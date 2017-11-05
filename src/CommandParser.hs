{-# LANGUAGE OverloadedStrings #-}

module CommandParser
  ( Command (..)
  , parseCommand
  ) where

import           Control.Applicative  ((<|>))
import           Data.Attoparsec.Text (Parser, endOfInput, inClass, notChar,
                                       notInClass, parseOnly, satisfy, skipMany,
                                       string, takeWhile, try)
import           Data.Eq              (Eq)
import           Data.Text            (Text, pack)
import           Prelude              (Char, Either, Show, String, ($), (*>),
                                       (.), (<$>), (<*))

data Command = Eval Text
             | Type Text
             deriving (Show)

parseCommand :: Text -> Either String Command
parseCommand = parseOnly parseCommandInternal

parseCommandInternal :: Parser Command
parseCommandInternal
  = try parseEval <|> parseType

parseEval :: Parser Command
parseEval
  = skipMany spaceOrNewline
  *> try (string ">")
  *> skipMany spaceOrNewline
  *> (Eval <$> parseCode)
  <* skipMany spaceOrNewline
  <* endOfInput

parseType :: Parser Command
parseType
  = skipMany spaceOrNewline
  *> try (string ":t")
  *> skipMany spaceOrNewline
  *> (Type <$> parseCode)
  <* skipMany spaceOrNewline
  <* endOfInput

spaceOrNewline :: Parser Char
spaceOrNewline = satisfy $ inClass " \n`"

parseCode :: Parser Text
parseCode = takeWhile $ notInClass "`"

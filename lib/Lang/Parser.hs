{-# LANGUAGE OverloadedStrings #-}

module Lang.Parser (parseProgram) where

import Data.Text (Text)
import Data.Void (Void)
import Lang.Lexer (whitespace)
import Lang.Syntax (Decl, Located (At), Program (..))
import Text.Megaparsec
    ( MonadParsec (eof)
    , ParseErrorBundle
    , Parsec
    , getSourcePos
    , many
    , runParser
    )

type Parser = Parsec Void Text

type ParseResult a = Either (ParseErrorBundle Text Void) a

parseProgram :: FilePath -> Text -> ParseResult Program
parseProgram = runParser program

program :: Parser Program
program = whitespace *> (Program <$> many (located decl)) <* eof

located :: Parser a -> Parser (Located a)
located p = At <$> getSourcePos <*> p

decl :: Parser Decl
decl = undefined

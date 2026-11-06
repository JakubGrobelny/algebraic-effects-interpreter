{-# LANGUAGE OverloadedStrings #-}

module Lang.Lexer (whitespace) where

import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (space1)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

whitespace :: Parser ()
whitespace = L.space space1 (L.skipLineComment "#") (L.skipBlockComment "#[" "]#")

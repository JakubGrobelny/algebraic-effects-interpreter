{-# LANGUAGE OverloadedStrings #-}

module Lang.Parser (parseProgram) where

import Data.Text (Text)
import Data.Void (Void)
import Lang.Syntax
import Text.Megaparsec

type ParseResult a = Either (ParseErrorBundle Text Void) a

parseProgram :: FilePath -> Text -> ParseResult Program
parseProgram = undefined

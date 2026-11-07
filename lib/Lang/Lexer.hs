{-# LANGUAGE OverloadedStrings #-}

module Lang.Lexer
    ( whitespace
    , keyword
    , bar
    , assign
    , lowerIdent
    , upperIdent
    , symbol
    , operator
    , wildcard
    , integer
    , stringLiteral
    ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Lang.Syntax (Name)
import Text.Megaparsec
    ( MonadParsec (notFollowedBy, try)
    , Parsec
    , empty
    , getOffset
    , manyTill
    , oneOf
    , region
    , satisfy
    , setErrorOffset
    , takeWhileP
    , (<?>)
    )
import Text.Megaparsec.Char (char, space1, string)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

whitespace :: Parser ()
whitespace = L.space space1 (L.skipLineComment "#") empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme whitespace

isIdentChar :: Char -> Bool
isIdentChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '\''

keyword :: Text -> Parser ()
keyword kw = (lexeme . try) (string kw *> notFollowedBy (satisfy isIdentChar))

operator :: Text -> Parser ()
operator s = (lexeme . try) (string s *> notFollowedBy (oneOf longer))
  where
    longer = fromMaybe "" (lookup s extensions)
    extensions :: [(Text, String)]
    extensions = [("=", "="), ("|", "|"), ("-", ">"), ("+", "+"), ("<", "="), (">", "=")]

bar :: Parser ()
bar = operator "|"

assign :: Parser ()
assign = operator "="

lowerIdent :: Parser Name
lowerIdent = identifier isAsciiLower "identifier"

upperIdent :: Parser Name
upperIdent = identifier isAsciiUpper "constructor"

keywords :: [Text]
keywords =
    [ "let"
    , "rec"
    , "in"
    , "fun"
    , "if"
    , "then"
    , "else"
    , "match"
    , "handle"
    , "with"
    , "return"
    , "end"
    , "type"
    , "effect"
    ]

identifier :: (Char -> Bool) -> String -> Parser Name
identifier start what = lexeme (try ident) <?> what
  where
    ident = do
        o <- getOffset
        c <- satisfy start
        rest <- takeWhileP Nothing isIdentChar
        let name = T.cons c rest
        if name `elem` keywords
            then
                region (setErrorOffset o) (fail ("unexpected keyword " ++ show (T.unpack name)))
            else pure name

symbol :: Text -> Parser ()
symbol s = () <$ L.symbol whitespace s

wildcard :: Parser ()
wildcard =
    (lexeme . try) (char '_' *> notFollowedBy (satisfy isIdentChar)) <?> "_"

integer :: Parser Integer
integer =
    lexeme (L.decimal <* notFollowedBy (satisfy isIdentChar)) <?> "integer"

stringLiteral :: Parser Text
stringLiteral = lexeme (T.pack <$> body) <?> "string"
  where
    body = char '"' *> manyTill L.charLiteral (char '"')

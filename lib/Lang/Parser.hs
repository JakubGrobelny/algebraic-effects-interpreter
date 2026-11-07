{-# LANGUAGE OverloadedStrings #-}

module Lang.Parser (parseProgram) where

import Control.Applicative (empty)
import Data.Text (Text)
import Data.Void (Void)
import Lang.Lexer
import Lang.Syntax
import Text.Megaparsec
    ( MonadParsec (eof)
    , ParseErrorBundle
    , Parsec
    , choice
    , getSourcePos
    , many
    , option
    , optional
    , runParser
    , sepBy
    , sepBy1
    , some
    , (<?>)
    , (<|>)
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
decl =
    choice
        [ typeDecl
        , effectDecl
        -- TODO: let declaration
        ]
        <?> "declaration"

typeDecl :: Parser Decl
typeDecl =
    keyword "type"
        *> ( TypeDecl
                <$> upperIdent
                <*> many lowerIdent
                <*> option [] (assign *> some ctor)
           )
  where
    ctor = bar *> (Ctor <$> upperIdent <*> many atomicType) <?> "constructor"

arrowType :: Parser Type
arrowType = do
    a <- applicationType
    option a (FunType a <$> (operator "->" *> optional row) <*> arrowType)

applicationType :: Parser Type
applicationType = (ConType <$> upperIdent <*> many atomicType) <|> atomicType

atomicType :: Parser Type
atomicType =
    choice
        [ typeVariable
        , nullaryType
        , parenType
        ]
        <?> "type"
  where
    typeVariable = VarType <$> lowerIdent
    nullaryType = (\c -> ConType c []) <$> upperIdent
    parenType = do
        symbol "("
        t <- arrowType
        ts <- sepBy arrowType (symbol ",")
        symbol ")"
        return $ case ts of
            [] -> t
            _ -> TupleType (t : ts)

row :: Parser Row
row = symbol "<" *> body <* symbol ">"
  where
    body =
        choice
            [ Row [] . Just <$> lowerIdent
            , Row <$> sepBy1 effect (symbol ",") <*> optional (bar *> lowerIdent)
            , return (Row [] Nothing)
            ]
    effect = Effect <$> upperIdent <*> many atomicType

effectDecl :: Parser Decl
effectDecl =
    keyword "effect"
        *> (EffectDecl <$> upperIdent <*> many lowerIdent <* operator "=" <*> some opSig)
  where
    opSig =
        bar
            *> ( OpSig
                    <$> lowerIdent
                    <* operator ":"
                    <*> applicationType
                    <* operator "->"
                    <*> arrowType
               )
            <?> "operation signature"

{-# LANGUAGE OverloadedStrings #-}

module Lang.Parser (parseProgram) where

import Control.Applicative (empty)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Either (partitionEithers)
import Data.Text (Text)
import Data.Void (Void)
import Lang.Lexer
import Lang.Syntax
import Text.Megaparsec
    ( MonadParsec (eof)
    , ParseErrorBundle
    , Parsec
    , choice
    , getOffset
    , getSourcePos
    , many
    , option
    , optional
    , region
    , runParser
    , sepBy1
    , setErrorOffset
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
        [ typeDecl <?> "type declaration"
        , effectDecl <?> "effect declaration"
        , letDecl <?> "let declaration"
        ]

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
        ts <- many (symbol "," *> arrowType)
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

letDecl :: Parser Decl
letDecl = keyword "let" *> (LetDecl <$> binding)

binding :: Parser Binding
binding = funBinding <|> patternBinding
  where
    funBinding = do
        recursive <- option NonRec (Rec <$ keyword "rec")
        name <- lowerIdent
        params <- many atomicPattern
        annotation <- optional typeAnnotation
        assign
        body <- expr
        return $ FunBinding name recursive params annotation body
    patternBinding =
        PatternBinding
            <$> pattern
            <*> optional typeAnnotation
            <*> (assign *> expr)

typeAnnotation :: Parser Type
typeAnnotation = operator ":" *> arrowType

expr :: Parser Expr
expr = choice [letExpr, lambdaExpr, sequenceExpr] <?> "expression"
  where
    letExpr =
        LetExpr
            <$> (keyword "let" *> binding)
            <*> (keyword "in" *> expr)
    lambdaExpr =
        LambdaExpr
            <$> (keyword "fun" *> some atomicPattern)
            <*> (operator "->" *> expr)
    sequenceExpr = do
        first <- conditionalExpr
        option first (SeqExpr first <$> (symbol ";" *> expr))

conditionalExpr :: Parser Expr
conditionalExpr = ifExpr <|> operatorExpr
  where
    ifExpr =
        IfExpr
            <$> (keyword "if" *> expr)
            <*> (keyword "then" *> expr)
            <*> (keyword "else" *> conditionalExpr)

operatorExpr :: Parser Expr
operatorExpr =
    makeExprParser
        applicationExpr
        [ [infixLeft MulOp, infixLeft DivOp, infixLeft ModOp]
        , [infixLeft AddOp, infixLeft SubOp]
        , [infixRight ConcatOp]
        , map infixNone [EqOp, NeqOp, LtOp, LeOp, GtOp, GeOp]
        , [infixRight AndOp]
        , [infixRight OrOp]
        ]
  where
    binaryOp op = BinOpExpr op <$ operator (binOpSymbol op)
    infixLeft = InfixL . binaryOp
    infixRight = InfixR . binaryOp
    infixNone = InfixN . binaryOp

applicationExpr :: Parser Expr
applicationExpr = foldl ApExpr <$> atomicExpr <*> many atomicExpr

atomicExpr :: Parser Expr
atomicExpr =
    choice
        [ VarExpr <$> lowerIdent
        , ConExpr <$> upperIdent
        , LiteralExpr <$> literal
        , parenExpr
        , matchExpr
        , handleExpr
        ]

literal :: Parser Literal
literal = (IntLiteral <$> integer) <|> (StringLiteral <$> stringLiteral)

parenExpr :: Parser Expr
parenExpr = symbol "(" *> (unitExpr <|> innerExpr)
  where
    unitExpr = LiteralExpr UnitLiteral <$ symbol ")"
    innerExpr = do
        first <- expr
        result <-
            choice
                [ AnnotatedExpr first <$> typeAnnotation
                , TupleExpr . (first :) <$> some (symbol "," *> expr)
                , return first
                ]
        symbol ")"
        return result

matchExpr :: Parser Expr
matchExpr =
    MatchExpr
        <$> (keyword "match" *> expr)
        <*> (keyword "with" *> many matchArm <* keyword "end")
  where
    matchArm = (,) <$> (bar *> pattern) <*> (operator "->" *> expr)

handleExpr :: Parser Expr
handleExpr =
    HandleExpr
        <$> (keyword "handle" *> expr)
        <*> (keyword "with" *> handler <* keyword "end")

handler :: Parser Handler
handler = do
    clauses <- some (bar *> clause)
    let (returnClauses, opClauses) = partitionEithers clauses
    case returnClauses of
        [] -> return $ Handler Nothing opClauses
        [(_, ret)] -> return $ Handler (Just ret) opClauses
        _ : (offset, _) : _ ->
            region (setErrorOffset offset) $
                fail "a handler can only have one return clause"
  where
    clause = (Left <$> returnClause) <|> (Right <$> opClause)
    returnClause = do
        offset <- getOffset
        keyword "return"
        value <- pattern
        operator "->"
        body <- expr
        return (offset, (value, body))
    opClause =
        OpClause
            <$> lowerIdent
            <*> atomicPattern
            <*> continuation
            <*> (operator "->" *> expr)
    continuation =
        (Just <$> lowerIdent) <|> (Nothing <$ wildcard) <?> "continuation"

pattern :: Parser Pattern
pattern =
    (ConPattern <$> upperIdent <*> many atomicPattern) <|> atomicPattern
        <?> "pattern"

atomicPattern :: Parser Pattern
atomicPattern =
    choice
        [ VarPattern <$> lowerIdent
        , WildcardPattern <$ wildcard
        , nullaryConPattern
        , LiteralPattern <$> literal
        , parenPattern
        ]
        <?> "pattern"
  where
    nullaryConPattern = (\c -> ConPattern c []) <$> upperIdent
    parenPattern = symbol "(" *> (unitPattern <|> innerPattern)
    unitPattern = LiteralPattern UnitLiteral <$ symbol ")"
    innerPattern = do
        first <- pattern
        result <-
            option first (TuplePattern . (first :) <$> some (symbol "," *> pattern))
        symbol ")"
        return result

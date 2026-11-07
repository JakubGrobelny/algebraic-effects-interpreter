{-# LANGUAGE OverloadedStrings #-}

module Lang.Syntax
    ( Located (..)
    , Name
    , Program (..)
    , Decl (..)
    , Ctor (..)
    , OpSig (..)
    , Rec (..)
    , Binding (..)
    , Expr (..)
    , Literal (..)
    , BinOp (..)
    , binOpSymbol
    , Handler (..)
    , OpClause (..)
    , Pattern (..)
    , Type (..)
    , Row (..)
    , Effect (..)
    ) where

import Data.Text (Text)
import Text.Megaparsec (SourcePos)

data Located a = At {loc :: SourcePos, unLoc :: a}
    deriving (Eq, Show)

type Name = Text

newtype Program = Program [Located Decl]
    deriving (Eq, Show)

data Decl
    = TypeDecl Name [Name] [Ctor]
    | EffectDecl Name [Name] [OpSig]
    | LetDecl Binding
    deriving (Eq, Show)

data Ctor = Ctor Name [Type]
    deriving (Eq, Show)

data OpSig = OpSig Name Type Type
    deriving (Eq, Show)

data Rec = NonRec | Rec
    deriving (Eq, Show)

data Binding
    = FunBinding Name Rec [Pattern] (Maybe Type) Expr
    | PatternBinding Pattern (Maybe Type) Expr
    deriving (Eq, Show)

data Expr
    = VarExpr Name
    | ConExpr Name
    | LiteralExpr Literal
    | ApExpr Expr Expr
    | BinOpExpr BinOp Expr Expr
    | LambdaExpr [Pattern] Expr
    | LetExpr Binding Expr
    | IfExpr Expr Expr Expr
    | SeqExpr Expr Expr
    | TupleExpr [Expr]
    | AnnotatedExpr Expr Type
    | MatchExpr Expr [(Pattern, Expr)]
    | HandleExpr Expr Handler
    deriving (Eq, Show)

data Literal
    = IntLiteral Integer
    | StringLiteral Text
    | UnitLiteral
    deriving (Eq, Show)

data BinOp
    = OrOp
    | AndOp
    | EqOp
    | NeqOp
    | LtOp
    | LeOp
    | GtOp
    | GeOp
    | ConcatOp
    | AddOp
    | SubOp
    | MulOp
    | DivOp
    | ModOp
    deriving (Eq, Show)

binOpSymbol :: BinOp -> Text
binOpSymbol OrOp = "||"
binOpSymbol AndOp = "&&"
binOpSymbol EqOp = "=="
binOpSymbol NeqOp = "!="
binOpSymbol LtOp = "<"
binOpSymbol LeOp = "<="
binOpSymbol GtOp = ">"
binOpSymbol GeOp = ">="
binOpSymbol ConcatOp = "++"
binOpSymbol AddOp = "+"
binOpSymbol SubOp = "-"
binOpSymbol MulOp = "*"
binOpSymbol DivOp = "/"
binOpSymbol ModOp = "%"

data Handler = Handler
    { handlerReturn :: Maybe (Pattern, Expr)
    , handlerOps :: [OpClause]
    }
    deriving (Eq, Show)

data OpClause = OpClause
    { opClauseName :: Name
    , opClausePattern :: Pattern
    , opClauseCont :: Maybe Name
    , opClauseBody :: Expr
    }
    deriving (Eq, Show)

data Pattern
    = VarPattern Name
    | WildcardPattern
    | ConPattern Name [Pattern]
    | LiteralPattern Literal
    | TuplePattern [Pattern]
    deriving (Eq, Show)

data Type
    = VarType Name
    | ConType Name [Type]
    | TupleType [Type]
    | FunType Type (Maybe Row) Type
    deriving (Eq, Show)

data Row = Row [Effect] (Maybe Name)
    deriving (Eq, Show)

data Effect = Effect Name [Type]
    deriving (Eq, Show)

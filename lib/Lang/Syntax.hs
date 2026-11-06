module Lang.Syntax where

import Data.Text (Text)
import Text.Megaparsec (SourcePos)

data Located a = At { loc :: SourcePos, unLoc :: a }
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
  = FunBinding Name Rec [Name] (Maybe Type) Expr
  | PatternBinding Pattern (Maybe Type) Expr
  deriving (Eq, Show)

data Expr

data Pattern

data Type

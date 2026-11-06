module Lang.Syntax where

import Text.Megaparsec (SourcePos)

data Located a = At { loc :: SourcePos, unLoc :: a }
  deriving (Eq, Show)

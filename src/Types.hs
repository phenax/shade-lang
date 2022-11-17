module Types where

import Data.Void (Void)
import qualified Text.Megaparsec as MP

type Parser = MP.Parsec Void String

data Literal = LString String | LInt Int
  deriving (Show, Eq)

data IdentifierType = VariableName | TypeName

newtype Identifier (t :: IdentifierType) = Identifier String
  deriving (Show, Eq)

data Expr
  = ELiteral Literal
  | ELambda (Identifier 'VariableName) Expr
  | EApply Expr Expr
  | EVariable (Identifier 'VariableName)
  deriving (Show, Eq)

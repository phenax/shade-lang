module Types where

import Data.Void (Void)
import qualified Text.Megaparsec as MP

type Parser = MP.Parsec Void String

data Literal = LString String | LInt Int
  deriving (Show, Eq)

newtype Identifier = IVariable String
  deriving (Show, Eq)

data Expr
  = ELiteral Literal
  | ELambda Identifier Expr
  | EApply Expr Expr
  | EVariable Identifier
  deriving (Show, Eq)

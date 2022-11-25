module Types where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Void (Void)
import qualified Text.Megaparsec as MP

type Parser = MP.Parsec Void String

data Literal = LString String | LInt Int | LBool Bool | LUnit
  deriving (Show, Eq)

data IdentifierType = VariableName | TypeName | TypeVarName | CtorName
  deriving (Show, Eq)

newtype Identifier (t :: IdentifierType) = Identifier {getIdentName :: String}
  deriving (Show, Eq, Ord)

data Expr
  = ELiteral Literal
  | ELambda (Identifier 'VariableName) Expr
  | EApply Expr Expr
  | EVariable (Identifier 'VariableName)
  | EIfElse Expr Expr Expr
  deriving (Show, Eq)

data Ctor = Ctor (Identifier 'CtorName) [Type]
  deriving (Show, Eq)

data Declr
  = Definition (Identifier 'VariableName) Expr
  | Declaration (Identifier 'VariableName) Scheme
  | TypeDefinition (Identifier 'TypeName) Scheme
  | DataDefinition (Identifier 'TypeName) [Ctor]
  deriving (Show, Eq)

newtype Module = Module [Declr]

data Type
  = TVariable String
  | TInt
  | TString
  | TBool
  | TUnit
  | TLambda Type Type
  deriving (Show, Eq)

data Scheme = Scheme [Identifier 'TypeVarName] Type
  deriving (Show, Eq)

type Subst = Map.Map String Type

class TypeOperations a where
  ftv :: a -> Set.Set String
  apply :: Subst -> a -> a

newtype TypeEnv = TypeEnv (Map.Map (Identifier 'VariableName) Scheme)

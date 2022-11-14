module Parser where

import Control.Applicative ((<|>))
import Data.Void (Void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import Text.Megaparsec.Error (ParseErrorBundle)

type Parser = MP.Parsec Void String

data Literal = LString String | LInt Int
  deriving (Show, Eq)

newtype Identifier = IVariable String
  deriving (Show, Eq)

data Expr
  = ELiteral Literal
  | ELambda Identifier Expr
  | EVariable Identifier
  deriving (Show, Eq)

parseInt :: Parser Int
parseInt = read <$> MP.some MP.digitChar

parseString :: Parser String
parseString = do
  _ <- MP.char '"'
  MP.manyTill MP.latin1Char (MP.char '"')

parseLiteral :: Parser Literal
parseLiteral = (LInt <$> parseInt) <|> (LString <$> parseString)

parseIdent :: Parser String
parseIdent = do
  first <- MP.letterChar
  rest <- MP.some MP.alphaNumChar
  pure $ first : rest

parseVar :: Parser Identifier
parseVar = IVariable <$> parseIdent

parseExpression :: Parser Expr
parseExpression = (EVariable <$> parseVar) <|> (ELiteral <$> parseLiteral)

-- parse :: String -> Either (ParseErrorBundle String Void) String
-- parse = MP.runParser parseExpression "mafile"

-- Expression
-- Blocks
--
-- hello : String
-- hello = "whatever"
--
-- main = print "wow"

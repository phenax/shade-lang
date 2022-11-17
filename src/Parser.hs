module Parser where

import Control.Monad (void)
import Control.Monad.Combinators ((<|>))
import Data.Foldable (Foldable (fold))
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Text.Megaparsec.Error (ParseErrorBundle)
import Types

parseInt :: Parser Int
parseInt = read <$> MP.some MP.digitChar

parseString :: Parser String
parseString = do
  _ <- MP.char '"'
  MP.manyTill MP.latin1Char (MP.char '"')

parseLiteral :: Parser Literal
parseLiteral = lexeme p
  where
    p = (LString <$> parseString) <|> (LInt <$> parseInt)

parseIdent :: Parser (Identifier a)
parseIdent = lexeme $ do
  first <- MP.letterChar
  rest <- MP.many MP.alphaNumChar
  pure $ Identifier $ first : rest

parseVar :: Parser (Identifier 'VariableName)
parseVar = parseIdent

spaceConsumer :: Parser ()
spaceConsumer =
  MPL.space
    MP.space1
    (MPL.skipLineComment "--")
    (MPL.skipBlockComment "/*" "*/")

symbol :: String -> Parser String
symbol = MPL.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme = MPL.lexeme spaceConsumer

parens :: Parser a -> Parser a
parens = MP.between (symbol "(") (symbol ")")

parseLambda :: Parser Expr
parseLambda = lexeme $ do
  _ <- symbol "\\"
  idents <- MP.someTill parseVar (symbol "->")
  body <- parseExpression
  pure $ foldl (flip ELambda) body (reverse idents)

parseRawExpr :: Parser Expr
parseRawExpr =
  parseLambda
    <|> (EVariable <$> parseVar)
    <|> (ELiteral <$> parseLiteral)

parseExpression :: Parser Expr
parseExpression = parens parseRawExpr <|> parseRawExpr

-- parse :: String -> Either (ParseErrorBundle String Void) String
-- parse = MP.runParser parseExpression "mafile"

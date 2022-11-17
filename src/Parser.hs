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
    (void $ MP.oneOf " \t")
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
  pure $ foldr ELambda body idents

argListP :: Parser e -> Parser [e]
argListP argP = argListParser []
  where
    argListParser ls = do
      optn <- MP.optional . MP.try $ spaceConsumer >> argP
      spaceConsumer
      case optn of
        Nothing -> pure ls
        Just p -> argListParser $ ls ++ [p]

parseApply :: Parser Expr
parseApply = lexeme $ do
  fn <- parseExprWithoutApply
  args <- argListP parseExprWithoutApply
  pure $ foldl EApply fn args

parseRawExpr :: Parser Expr
parseRawExpr =
  parseLambda
    <|> (EVariable <$> parseVar)
    <|> (ELiteral <$> parseLiteral)

parseExprWithoutApply :: Parser Expr
parseExprWithoutApply = parens parseApply <|> parseRawExpr <|> parseRawExpr

parseExpression :: Parser Expr
parseExpression = parseApply <|> parseExprWithoutApply

-- parse :: String -> Either (ParseErrorBundle String Void) String
-- parse = MP.runParser parseExpression "mafile"

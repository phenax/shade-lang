module Parser where

import Control.Monad (void)
import Control.Monad.Combinators ((<|>))
import Data.Foldable (Foldable (fold))
import Debug.Trace (trace)
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

scnl :: Parser ()
scnl =
  MPL.space
    (void MP.spaceChar)
    (MPL.skipLineComment "--")
    (MPL.skipBlockComment "/*" "*/")

sc :: Parser ()
sc =
  MPL.space
    (void $ MP.oneOf " \t")
    (MPL.skipLineComment "--")
    (MPL.skipBlockComment "/*" "*/")

symbol :: String -> Parser String
symbol = MPL.symbol scnl

lexeme :: Parser a -> Parser a
lexeme = MPL.lexeme scnl

parens :: Parser a -> Parser a
parens = MP.between (symbol "(") (symbol ")")

parseLambda :: Parser Expr
parseLambda = lexeme $ do
  _ <- symbol "\\"
  idents <- MP.someTill parseVar (symbol "->")
  body <- parseExpression
  pure $ foldr ELambda body idents

argListP :: Parser e -> Parser () -> Parser [e]
argListP argP spaceConsumer = argListParser []
  where
    argListParser ls = do
      optn <- MP.optional . MP.try $ spaceConsumer >> argP
      -- scnl
      case optn of
        Nothing -> pure ls
        Just p -> argListParser $ ls ++ [p]

withIndentGuard :: (Parser () -> Parser a) -> Parser a
withIndentGuard fn = do
  scnl
  level <- MPL.indentLevel
  trace (show level) (pure ())
  let sc' = void $ MPL.indentGuard sc GT level
  fn sc'

parseApply :: Parser Expr
parseApply = withIndentGuard $ \spaceConsumer -> do
  fn <- parseExprWithoutApply
  args <- argListP parseExprWithoutApply spaceConsumer
  pure $ foldl EApply fn args

parseRawExpr :: Parser Expr
parseRawExpr = parens (parseApply <|> p) <|> p
  where
    p =
      parseLambda
        <|> (EVariable <$> parseVar)
        <|> (ELiteral <$> parseLiteral)

parseExprWithoutApply :: Parser Expr
parseExprWithoutApply = parseRawExpr

parseExpression :: Parser Expr
parseExpression = (parseApply <|> parseExprWithoutApply) <* scnl

-- parse :: String -> Either (ParseErrorBundle String Void) String
-- parse = MP.runParser parseExpression "mafile"

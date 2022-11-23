module Syntax.Parser where

import Control.Monad (void)
import Control.Monad.Combinators ((<|>))
import Data.Foldable (Foldable (fold))
import Debug.Trace (trace)
import Syntax.Utils
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Text.Megaparsec.Error (ParseErrorBundle)
import Types

instance Parsable Int where
  parse = read <$> MP.some MP.digitChar

instance Parsable String where
  parse = do
    _ <- MP.char '"'
    MP.manyTill MP.latin1Char (MP.char '"')

instance Parsable Bool where
  parse = (True <$ symbol "True") <|> (False <$ symbol "False")

instance Parsable Literal where
  parse = lexeme p
    where
      p = (LString <$> parse) <|> (LInt <$> parse) <|> (LBool <$> parse)

instance Parsable (Identifier 'VariableName) where
  parse = lexeme $ do
    first <- MP.letterChar
    rest <- MP.many MP.alphaNumChar
    pure $ Identifier $ first : rest

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
  idents <- MP.someTill parse (symbol "->")
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
        <|> (ELiteral <$> parse)
        <|> (EVariable <$> (parse :: Parser (Identifier 'VariableName)))

parseExprWithoutApply :: Parser Expr
parseExprWithoutApply = parseRawExpr

parseExpression :: Parser Expr
parseExpression = (parseApply <|> parseExprWithoutApply) <* scnl

-- parse :: String -> Either (ParseErrorBundle String Void) String
-- parse = MP.runParser parseExpression "mafile"

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

symbol = MPL.symbol spaceConsumer

lexeme = MPL.lexeme spaceConsumer

parseLambda :: Parser Expr
parseLambda = lexeme $ do
  _ <- symbol "\\"
  idents <- MP.someTill parseVar (symbol "->")
  body <- parseExpression
  pure $ foldl (flip ELambda) body (reverse idents)

parseExpression :: Parser Expr
parseExpression =
  parseLambda
    <|> (EVariable <$> parseVar)
    <|> (ELiteral <$> parseLiteral)

-- parse :: String -> Either (ParseErrorBundle String Void) String
-- parse = MP.runParser parseExpression "mafile"

-- Expression
-- Blocks
--
-- hello : String
-- hello = "whatever"
--
-- main = print "wow"

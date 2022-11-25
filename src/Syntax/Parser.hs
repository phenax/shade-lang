module Syntax.Parser where

import Control.Monad (void)
import Control.Monad.Combinators ((<|>))
import qualified Data.Set as Set
import Syntax.TypeDef
import Syntax.Utils
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
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
  parse = parseLowerIdent

parseLambda :: Parser Expr
parseLambda = lexeme $ do
  _ <- symbol "\\"
  idents <- MP.someTill parse (symbol "->")
  body <- parse
  pure $ foldr ELambda body idents

withIndentGuard :: (Parser () -> Parser a) -> Parser a
withIndentGuard fn = do
  scnl
  level <- MPL.indentLevel
  let sc' = void $ MPL.indentGuard sc GT level
  fn sc'

parseApply :: Parser Expr
parseApply = withIndentGuard $ \spaceConsumer -> do
  fn <- parseExprWithoutApply
  args <- argListP parseExprWithoutApply spaceConsumer
  pure $ foldl EApply fn args

parseIfElse :: Parser Expr
parseIfElse = withIndentGuard $ \spaceConsumer -> do
  symbol "if"
  cond <- scnl >> parse
  symbol "then"
  thenE <- spaceConsumer >> parse
  symbol "else"
  elseE <- spaceConsumer >> parse
  pure $ EIfElse cond thenE elseE

parseRawExpr :: Parser Expr
parseRawExpr = parens (parseApply <|> p) <|> p
  where
    p =
      parseLambda
        <|> parseIfElse
        <|> (ELiteral <$> parse)
        <|> (EVariable <$> (parse :: Parser (Identifier 'VariableName)))

parseExprWithoutApply :: Parser Expr
parseExprWithoutApply = parseRawExpr

instance Parsable Expr where
  parse = (parseApply <|> parseExprWithoutApply) <* scnl

parseDefn :: Parser Declr
parseDefn = withIndentGuard $ \spaceConsumer -> do
  ident <- parse :: Parser (Identifier 'VariableName)
  args <- MP.many (parse :: Parser (Identifier 'VariableName))
  symbol "="
  body <- spaceConsumer >> parse
  let lambda = foldr ELambda body args
  return $ Definition ident lambda

parseDeclr :: Parser Declr
parseDeclr = withIndentGuard $ \spaceConsumer -> do
  ident <- parse :: Parser (Identifier 'VariableName)
  symbol "::"
  typ <- spaceConsumer >> (parse :: Parser Type)
  return $ Declaration ident (Scheme [] typ)

instance Parsable Declr where
  parse = scnl >> p <* scnl
    where
      p = MP.try parseDeclr <|> parseDefn

instance Parsable (Identifier 'ModuleName) where
  parse = parseUpperIdent

instance Parsable Module where
  parse = scnl >> p <* scnl
    where
      parseHeader = do
        modName <- symbol "module" >> (parse :: Parser (Identifier 'ModuleName))
        _ <- symbol "exposing" >> symbol "(..)"
        return $ ModuleHeader modName

      p = do
        header <- parseHeader <* scnl
        Module header <$> MP.many parse

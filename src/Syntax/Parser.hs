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
  parse _ = read <$> MP.some MP.digitChar

instance Parsable String where
  parse _ = do
    _ <- MP.char '"'
    MP.manyTill MP.latin1Char (MP.char '"')

instance Parsable Bool where
  parse _ = (True <$ symbol "True") <|> (False <$ symbol "False")

instance Parsable Literal where
  parse sc = lexeme p
    where
      p = (LString <$> parse sc) <|> (LInt <$> parse sc) <|> (LBool <$> parse sc)

instance Parsable (Identifier 'VariableName) where
  parse _ = parseLowerIdent

parseLambda :: Parser () -> Parser Expr
parseLambda sc = lexeme $ do
  _ <- symbol "\\"
  idents <- MP.someTill (parse sc) (symbol "->")
  body <- sc >> parse sc
  pure $ foldr ELambda body idents

withIndentGuard :: (Parser () -> Parser a) -> Parser a
withIndentGuard fn = do
  scnl
  level <- MPL.indentLevel
  let sc' = void $ MPL.indentGuard sc GT level
  fn sc'

parseApply :: Parser () -> Parser Expr
parseApply sc = do
  fn <- parseExprWithoutApply sc
  args <- argListP (parseExprWithoutApply sc) sc
  pure $ foldl EApply fn args

parseIfElse :: Parser () -> Parser Expr
parseIfElse sc = do
  symbol "if"
  cond <- scnl >> parse sc
  symbol "then"
  thenE <- sc >> parse sc
  symbol "else"
  elseE <- sc >> parse sc
  pure $ EIfElse cond thenE elseE

parseLetIn :: Parser () -> Parser Expr
parseLetIn sc = do
  symbol "let"
  bindings <- MP.many (sc >> parse sc) <* scnl
  symbol "in"
  expr <- parse sc
  pure $ ELetIn bindings expr

parseRawExpr :: Parser () -> Parser Expr
parseRawExpr sc = parens (parseApply sc <|> p) <|> p
  where
    p =
      parseLambda sc
        <|> parseIfElse sc
        <|> parseLetIn sc
        <|> (ELiteral <$> parse sc)
        <|> (EVariable <$> (parse sc :: Parser (Identifier 'VariableName)))

parseExprWithoutApply :: Parser () -> Parser Expr
parseExprWithoutApply = parseRawExpr

instance Parsable Expr where
  parse sc = (parseApply sc <|> parseExprWithoutApply sc) <* scnl

parseExpression :: Parser Expr
parseExpression = withIndentGuard parse

instance Parsable Binding where
  parse _ = withIndentGuard $
    \spaceConsumer ->
      MP.try (parseDeclr spaceConsumer) <|> MP.try (parseDefn spaceConsumer)

parseDefn :: Parser () -> Parser Binding
parseDefn sc = do
  ident <- parse sc :: Parser (Identifier 'VariableName)
  args <- MP.many (parse sc :: Parser (Identifier 'VariableName))
  symbol "="
  body <- sc >> parse sc
  let lambda = foldr ELambda body args
  return $ BindDefinition ident lambda

parseDeclr :: Parser () -> Parser Binding
parseDeclr sc = do
  ident <- parse sc :: Parser (Identifier 'VariableName)
  symbol "::"
  typ <- sc >> (parse sc :: Parser Type)
  return $ BindDeclaration ident (Scheme [] typ)

instance Parsable Declr where
  parse sc = scnl >> p <* scnl
    where
      p = Binding <$> parse sc

instance Parsable (Identifier 'ModuleName) where
  parse _ = parseUpperIdent

instance Parsable Module where
  parse sc = scnl >> p <* scnl
    where
      parseHeader = do
        modName <- symbol "module" >> (parse sc :: Parser (Identifier 'ModuleName))
        _ <- symbol "exposing" >> symbol "(..)"
        return $ ModuleHeader modName

      p = do
        header <- parseHeader <* scnl
        Module header <$> MP.many (parse sc)

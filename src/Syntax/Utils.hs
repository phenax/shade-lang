module Syntax.Utils where

import Control.Monad (void)
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Types

class Parsable a where
  parse :: Parser a

scnl :: Parser ()
sc :: Parser ()
(scnl, sc) =
  ( MPL.space (void MP.spaceChar) singleLC multiLC,
    MPL.space (void $ MP.oneOf " \t") singleLC multiLC
  )
  where
    singleLC = MPL.skipLineComment "--"
    multiLC = MPL.skipBlockComment "/*" "*/"

reservedKeywords :: [String]
reservedKeywords =
  [ "if",
    "then",
    "let",
    "else"
  ]

symbol :: String -> Parser String
symbol = MPL.symbol scnl

lexeme :: Parser a -> Parser a
lexeme = MPL.lexeme scnl

parens :: Parser a -> Parser a
parens = MP.between (symbol "(") (symbol ")")

argListP :: Parser e -> Parser () -> Parser [e]
argListP argP spaceConsumer = argListParser []
  where
    argListParser ls = do
      optn <- MP.optional . MP.try $ spaceConsumer >> argP
      -- scnl
      case optn of
        Nothing -> pure ls
        Just p -> argListParser $ ls ++ [p]

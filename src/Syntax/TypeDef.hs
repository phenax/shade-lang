module Syntax.TypeDef where

import Control.Monad (void)
import Control.Monad.Combinators ((<|>))
import Data.Foldable (Foldable (fold))
import qualified Data.Set as Set
import Debug.Trace (trace)
import Syntax.Utils
import qualified Text.Megaparsec as MP
import qualified Text.Megaparsec.Char as MP
import qualified Text.Megaparsec.Char.Lexer as MPL
import Text.Megaparsec.Error (ParseErrorBundle)
import Types

instance Parsable (Identifier 'TypeVarName) where
  parse _ = parseLowerIdent

instance Parsable (Identifier 'TypeName) where
  parse _ = parseUpperIdent

parseTypeName :: Parser Type
parseTypeName = lexeme $ do
  typeIdent <- parse scnl :: Parser (Identifier 'TypeName)
  return $ case typeIdent of
    Identifier "String" -> TString
    Identifier "Int" -> TInt
    Identifier "Bool" -> TBool
    _ -> TCustom typeIdent

parseTypeLambda :: Parser Type
parseTypeLambda = do
  typ <- parseTypeRecSafe
  trace (show typ) (pure ())
  typs <- MP.many (symbol "->" >> parseTypeRecSafe)
  return $ foldr1 TLambda (typ : typs)

parseTypeVarName :: Parser Type
parseTypeVarName = TVariable <$> (parse scnl :: Parser (Identifier 'TypeVarName))

parseRawType :: Parser Type
parseRawType = parseTypeRecSafe <|> parseTypeLambda

parseTypeRecSafe :: Parser Type
parseTypeRecSafe =
  parens parseRawType <|> parseTypeName <|> parseTypeVarName

instance Parsable Type where
  parse _ =
    scnl >> p <* scnl
    where
      p = parseTypeLambda <|> parseRawType

-- <|> parseTypeVar <|> parseTypeLambda
--
--
--
--
--
--

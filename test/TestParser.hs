module TestParser where

import Data.Either (isLeft)
import Parser (Expr (..), Identifier (..), Literal (..))
import qualified Parser
import Test.Hspec
import Text.Megaparsec (ParseErrorBundle (ParseErrorBundle))
import qualified Text.Megaparsec as MP

test :: SpecWith ()
test = do
  let p = MP.runParser (do e <- Parser.parseExpression; MP.eof; pure e) "mafile"

  describe "parse literals" $ do
    it "literals" $ do
      p "823232" `shouldBe` Right (ELiteral $ LInt 823232)
      p "\"hello world\"" `shouldBe` Right (ELiteral $ LString "hello world")

  describe "parse identifiers" $ do
    it "literals" $ do
      p "hello" `shouldBe` Right (EVariable $ IVariable "hello")
      p "h123" `shouldBe` Right (EVariable $ IVariable "h123")
      isLeft (p "12kashdl") `shouldBe` True

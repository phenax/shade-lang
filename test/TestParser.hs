module TestParser where

import Data.Bifunctor (first)
import Data.Either (fromLeft, isLeft)
import qualified Parser
import Test.Hspec
import qualified Text.Megaparsec as MP
import Types (Expr (..), Identifier (..), Literal (..))

test :: SpecWith ()
test = do
  let p = first MP.errorBundlePretty . MP.runParser (Parser.parseExpression <* MP.eof) "mafile"

  describe "parse literals" $ do
    it "literals" $ do
      p "823232" `shouldBe` Right (ELiteral $ LInt 823232)
      p "\"hello world\"" `shouldBe` Right (ELiteral $ LString "hello world")

  describe "parse identifiers" $ do
    it "vars" $ do
      p "hello" `shouldBe` Right (EVariable $ Identifier "hello")
      p "h123" `shouldBe` Right (EVariable $ Identifier "h123")
      p "x" `shouldBe` Right (EVariable $ Identifier "x")
      p "x " `shouldBe` Right (EVariable $ Identifier "x")
      isLeft (p "12kashdl") `shouldBe` True

  describe "parse lambda" $ do
    runIO $ putStrLn $ fromLeft "" $ p "\\x -> 5"
    it "lambda" $ do
      p "\\x -> 5" `shouldBe` Right (ELambda (Identifier "x") (ELiteral $ LInt 5))
      p "\\x y -> 5" `shouldBe` Right (ELambda (Identifier "x") (ELambda (Identifier "y") (ELiteral $ LInt 5)))

module TestParser where

import Data.Bifunctor (first)
import Data.Either (fromLeft, isLeft)
import qualified Syntax.Parser as Parser
import Test.Hspec
import qualified Text.Megaparsec as MP
import Text.RawString.QQ (r)
import Types (Expr (..), Identifier (..), Literal (..))

(~~>) :: String -> Expr -> Expr
(~~>) = ELambda . Identifier

infixr 9 ~~>

apply :: Expr -> Expr -> Expr
apply = EApply

var :: String -> Expr
var = EVariable . Identifier

test :: SpecWith ()
test = do
  let p = first MP.errorBundlePretty . MP.runParser (Parser.parseExpression <* MP.eof) "mafile"

  describe "parse literals" $ do
    it "literals" $ do
      p "823232" `shouldBe` Right (ELiteral $ LInt 823232)
      p "\"hello world\"" `shouldBe` Right (ELiteral $ LString "hello world")
      p "\"\"" `shouldBe` Right (ELiteral $ LString "")

  describe "parse identifiers" $ do
    it "vars" $ do
      p "hello" `shouldBe` Right (EVariable $ Identifier "hello")
      p "h123" `shouldBe` Right (EVariable $ Identifier "h123")
      p "x" `shouldBe` Right (EVariable $ Identifier "x")
      p "x " `shouldBe` Right (EVariable $ Identifier "x")
    -- isLeft (p "12kashdl") `shouldBe` True

      p "(hello)" `shouldBe` Right (EVariable $ Identifier "hello")
      p "(x)" `shouldBe` Right (EVariable $ Identifier "x")
      p "( y )" `shouldBe` Right (EVariable $ Identifier "y")
  -- isLeft (p "(12kjqhkdj)") `shouldBe` True

  describe "parse lambda" $ do
    it "lambda" $ do
      p "\\x -> 5" `shouldBe` Right ("x" ~~> ELiteral (LInt 5))
      p "\\x y -> 5" `shouldBe` Right ("x" ~~> "y" ~~> ELiteral (LInt 5))
    it "with parens" $ do
      p "\\x -> (5)" `shouldBe` Right ("x" ~~> ELiteral (LInt 5))
      p "(\\x y -> 5)" `shouldBe` Right ("x" ~~> ELambda (Identifier "y") (ELiteral $ LInt 5))

  describe "parse apply" $ do
    -- runIO $ putStrLn $ fromLeft "" $ p "hello 1 2"
    it "apply" $ do
      p "hello 1 2"
        `shouldBe` Right
          ( EVariable (Identifier "hello")
              `apply` ELiteral (LInt 1)
              `apply` ELiteral (LInt 2)
          )
      p "(\\x y -> add x y) 1 2"
        `shouldBe` Right
          ( ("x" ~~> "y" ~~> (var "add" `apply` var "x" `apply` var "y"))
              `apply` ELiteral (LInt 1)
              `apply` ELiteral (LInt 2)
          )
      p "hello (\\x -> x) 2"
        `shouldBe` Right
          ( var "hello"
              `apply` ("x" ~~> var "x")
              `apply` ELiteral (LInt 2)
          )
    it "with indents" $ do
      p
        [r|
  hello
    1
    2
          |]
        `shouldBe` Right
          ( EVariable (Identifier "hello")
              `apply` ELiteral (LInt 1)
              `apply` ELiteral (LInt 2)
          )

----
----
----
----
----
----
----
----
----
----
----

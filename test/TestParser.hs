module TestParser where

import Data.Bifunctor (first)
import Data.Either (fromLeft, isLeft)
import qualified Syntax.Parser as Parser
import qualified Syntax.TypeDef as Parser
import Syntax.Utils (scnl)
import qualified Syntax.Utils as Parser
import Test.Hspec
import qualified Text.Megaparsec as MP
import Text.RawString.QQ (r)
import Types (Declr (Declaration, Definition), Expr (..), Identifier (..), Literal (..), Module (..), ModuleHeader (..), Parser, Scheme (..), Type (..))

(~~>) :: String -> Expr -> Expr
(~~>) = ELambda . Identifier

infixr 9 ~~>

apply :: Expr -> Expr -> Expr
apply = EApply

var :: String -> Expr
var = EVariable . Identifier

tvar :: String -> Type
tvar = TVariable . Identifier

test :: SpecWith ()
test = do
  let p = first MP.errorBundlePretty . MP.runParser (Parser.parseExpression <* MP.eof) "mafile"

  describe "parse literals" $ do
    it "literals" $ do
      p "823232" `shouldBe` Right (ELiteral $ LInt 823232)
      p "\"hello world\"" `shouldBe` Right (ELiteral $ LString "hello world")
      p "\"\"" `shouldBe` Right (ELiteral $ LString "")
      p " True " `shouldBe` Right (ELiteral $ LBool True)
      p " False " `shouldBe` Right (ELiteral $ LBool False)

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
      p "elseFoobar" `shouldBe` Right (EVariable $ Identifier "elseFoobar")
    -- isLeft (p "(12kjqhkdj)") `shouldBe` True
    it "sad :(" $ do
      isLeft (p "if") `shouldBe` True
      isLeft (p "then") `shouldBe` True
      isLeft (p "else") `shouldBe` True

  describe "parse lambda" $
    do
      it "lambda" $ do
        p "\\x -> 5" `shouldBe` Right ("x" ~~> ELiteral (LInt 5))
        p "\\x y -> 5" `shouldBe` Right ("x" ~~> "y" ~~> ELiteral (LInt 5))
        p
          [r|
          \x y ->
            5
          |]
          `shouldBe` Right ("x" ~~> "y" ~~> ELiteral (LInt 5))
        isLeft
          ( p
              [r|
          \x y ->
          5
          |]
          )
          `shouldBe` True
      it "with parens" $ do
        p "\\x -> (5)" `shouldBe` Right ("x" ~~> ELiteral (LInt 5))
        p "(\\x y -> 5)" `shouldBe` Right ("x" ~~> ELambda (Identifier "y") (ELiteral $ LInt 5))

  describe "ifelse" $ do
    it "ifelse" $ do
      p
        [r| if True then "yes" else "no" |]
        `shouldBe` Right
          ( EIfElse
              (ELiteral $ LBool True)
              (ELiteral $ LString "yes")
              (ELiteral $ LString "no")
          )
      p
        [r|
        if True then
          "yes"
        else
          "no"
      |]
        `shouldBe` Right
          ( EIfElse
              (ELiteral $ LBool True)
              (ELiteral $ LString "yes")
              (ELiteral $ LString "no")
          )
      p
        [r|
        if
  True
          then
            "yes"
          else
            "no"
        |]
        `shouldBe` Right
          ( EIfElse
              (ELiteral $ LBool True)
              (ELiteral $ LString "yes")
              (ELiteral $ LString "no")
          )

    it "sad :(" $ do
      isLeft
        ( p
            [r|
            if True then
            "yes"
            else "no"
          |]
        )
        `shouldBe` True
      isLeft (p [r| if True |]) `shouldBe` True
      isLeft (p [r| if True then |]) `shouldBe` True
      isLeft (p [r| if True then True |]) `shouldBe` True

  describe "parse apply" $ do
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
    p
      [r|
        hello 1 (add 2
          5)
      |]
      `shouldBe` Right
        ( EVariable (Identifier "hello")
            `apply` ELiteral (LInt 1)
            `apply` ( EVariable (Identifier "add")
                        `apply` ELiteral (LInt 2)
                        `apply` ELiteral (LInt 5)
                    )
        )

  describe "declaration" $ do
    let pd = first MP.errorBundlePretty . MP.runParser ((Parser.parse scnl :: Parser Declr) <* MP.eof) "mafile"
    it "simple declaration" $ do
      pd [r|foobar :: String -> Int |]
        `shouldBe` Right
          ( Declaration (Identifier "foobar") (Scheme [] $ TString `TLambda` TInt)
          )
    it "simple definiton" $ do
      pd [r|foobar = 200 |] `shouldBe` Right (Definition (Identifier "foobar") (ELiteral $ LInt 200))
      pd
        [r|
foobar =
  hello
    world
  |]
        `shouldBe` Right (Definition (Identifier "foobar") (var "hello" `apply` var "world"))
      pd [r|foobar a b = 200 |]
        `shouldBe` Right
          ( Definition
              (Identifier "foobar")
              ("a" ~~> "b" ~~> ELiteral (LInt 200))
          )
    it "sad case :(" $ do
      isLeft
        ( pd
            [r|
foobar a b =
200
      |]
        )
        `shouldBe` True

  describe "typedef" $ do
    let pt = first MP.errorBundlePretty . MP.runParser ((Parser.parse scnl :: Parser Type) <* MP.eof) "mafile"
    it "simple type defn" $ do
      pt [r| String |] `shouldBe` Right TString
      pt [r| Int |] `shouldBe` Right TInt
      pt [r| Bool |] `shouldBe` Right TBool
      pt [r| Custommm |] `shouldBe` Right (TCustom $ Identifier "Custommm")
    it "type lambda" $ do
      pt [r| Int -> String -> Bool |] `shouldBe` Right (TInt `TLambda` (TString `TLambda` TBool))
      pt [r| Int -> String |] `shouldBe` Right (TInt `TLambda` TString)
      pt
        [r|
          Int
        -> String
        -> PP
        |]
        `shouldBe` Right (TInt `TLambda` (TString `TLambda` TCustom (Identifier "PP")))
      pt [r| a -> b -> foobar |] `shouldBe` Right (tvar "a" `TLambda` (tvar "b" `TLambda` tvar "foobar"))
    it "type var" $ do
      pt [r| a |] `shouldBe` Right (tvar "a")
      pt [r| bad |] `shouldBe` Right (tvar "bad")

  describe "module" $ do
    let pd = first MP.errorBundlePretty . MP.runParser ((Parser.parse scnl :: Parser Module) <* MP.eof) "mafile"
    it "simple module" $ do
      pd
        [r|
        module Foobar exposing (..)

        foobar :: a -> Int 
        foobar x = 200
        |]
        `shouldBe` Right
          ( Module
              (ModuleHeader $ Identifier "Foobar")
              [ Declaration (Identifier "foobar") (Scheme [] (tvar "a" `TLambda` TInt)),
                Definition (Identifier "foobar") ("x" ~~> ELiteral (LInt 200))
              ]
          )

----

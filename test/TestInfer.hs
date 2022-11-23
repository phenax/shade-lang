module TestInfer where

import Data.Either (fromLeft, isLeft)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec
import Text.RawString.QQ (r)
import TypeChecker.Infer
import TypeChecker.TIMonad
import Types

(~~>) :: String -> Expr -> Expr
(~~>) = ELambda . Identifier

infixr 9 ~~>

test :: SpecWith ()
test = do
  let infer env e = fst <$> runTI (typeInference env e)
  describe "literal/identifiers" $ do
    it "identifier" $ do
      let e = EVariable (Identifier "foobar")
      let env = TypeEnv $ Map.singleton (Identifier "foobar") (Scheme [] TInt)
      infer env e `shouldReturn` Right TInt

  describe "lambda" $ do
    it "lambda" $ do
      let e = "foo" ~~> EVariable (Identifier "foo")
      let env = TypeEnv Map.empty
      infer env e `shouldReturn` Right (TVariable "a0" `TLambda` TVariable "a0")

  describe "apply" $ do
    it "functon application" $ do
      let e = ("x" ~~> EVariable (Identifier "x")) `EApply` ELiteral (LInt 5)
      let env = TypeEnv Map.empty
      infer env e `shouldReturn` Right TInt

  describe "if-else" $ do
    it "if-else" $ do
      let e =
            EIfElse
              (EVariable $ Identifier "cond")
              (EVariable $ Identifier "thenE")
              (EVariable $ Identifier "elseE")
      let env =
            TypeEnv $
              Map.fromList
                [ (Identifier "cond", Scheme [] TBool),
                  (Identifier "thenE", Scheme [] TString),
                  (Identifier "elseE", Scheme [] TString)
                ]
      infer env e `shouldReturn` Right TString
    it "if-else" $ do
      let e =
            EIfElse
              (EVariable (Identifier "cond") `EApply` ELiteral (LInt 5))
              (EVariable $ Identifier "thenE")
              (EVariable $ Identifier "elseE")
      let env =
            TypeEnv $
              Map.fromList
                [ (Identifier "cond", Scheme [] $ TInt `TLambda` TBool),
                  (Identifier "thenE", Scheme [] $ TVariable "b" `TLambda` TInt),
                  (Identifier "elseE", Scheme [] $ TString `TLambda` TInt)
                ]
      infer env e `shouldReturn` Right (TLambda TString TInt)
    it "sad :(" $ do
      let e =
            EIfElse
              (ELiteral (LString "foobar"))
              (ELiteral (LString "foobar"))
              (ELiteral (LString "foobar"))
      isLeft <$> infer (TypeEnv Map.empty) e `shouldReturn` True
    it "sad :(" $ do
      let e =
            EIfElse
              (ELiteral (LBool True))
              (ELiteral (LString "foobar"))
              (ELiteral (LInt 5))
      isLeft <$> infer (TypeEnv Map.empty) e `shouldReturn` True
    it "sad :(" $ do
      let e =
            EIfElse
              (ELiteral (LBool True))
              (ELiteral (LInt 5))
              (ELiteral (LString "foobar"))
      isLeft <$> infer (TypeEnv Map.empty) e `shouldReturn` True

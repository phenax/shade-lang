module TestInfer where

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
  describe "infer literal" $ do
    let infer env e = fst <$> runTI (typeInference env e)

    it "should do shit" $ do
      let e = EVariable (Identifier "foobar")
      let env = TypeEnv $ Map.singleton (Identifier "foobar") (Scheme [] TInt)
      infer env e `shouldReturn` Right TInt

    it "should do shit" $ do
      let e = "foo" ~~> EVariable (Identifier "foo")
      let env = TypeEnv Map.empty
      infer env e `shouldReturn` Right (TVariable "a0" `TLambda` TVariable "a0")

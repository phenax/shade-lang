module TestModuleTypeCheck where

import Data.Either (fromLeft, isLeft)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Test.Hspec
import Text.RawString.QQ (r)
import TypeChecker.Infer
import TypeChecker.ModuleCheck (checkModule)
import TypeChecker.TIMonad
import Types

(~~>) :: String -> Expr -> Expr
(~~>) = ELambda . Identifier

infixr 9 ~~>

(<::>) :: String -> Type -> Declr
(<::>) name ty = Binding $ BindDeclaration (Identifier name) (Scheme [] ty)

infixr 1 <::>

(<=>) :: String -> Expr -> Declr
(<=>) name = Binding . BindDefinition (Identifier name)

infixr 1 <=>

test :: SpecWith ()
test = do
  let checkM e = fst <$> runTI (checkModule e)
  describe "module type check" $ do
    it "moddul" $ do
      let modul =
            Module
              (ModuleHeader (Identifier "MyMod"))
              [ "foobar" <=> ELiteral LUnit,
                "foobar" <::> TUnit,
                "mul" <::> TInt `TLambda` (TInt `TLambda` TInt),
                "mul3" <::> TInt `TLambda` TInt,
                "mul3"
                  <=> "x"
                  ~~> ( ( EVariable (Identifier "mul")
                            `EApply` EVariable (Identifier "x")
                        )
                          `EApply` ELiteral (LInt 3)
                      )
              ]
      let env =
            TypeEnv $
              Map.fromList
                [ (Identifier "foobar", Scheme [] TUnit),
                  (Identifier "mul", Scheme [] $ TInt `TLambda` (TInt `TLambda` TInt)),
                  (Identifier "mul3", Scheme [] $ TInt `TLambda` TInt)
                ]
      checkM modul `shouldReturn` Right env

---
---
---
---
---
---
---
---
---
---

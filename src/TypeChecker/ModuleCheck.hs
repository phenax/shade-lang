module TypeChecker.ModuleCheck where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import TypeChecker.Infer
import TypeChecker.TIMonad
import Types

-- data Export
--   = ExportValue (Identifier 'VariableName) Scheme
--   | ExportType (Identifier 'TypeName) Scheme

accumulateDeclrs :: TypeEnv -> Declr -> TIMonad TypeEnv
accumulateDeclrs (TypeEnv env) (Binding (BindDeclaration name scheme)) = do
  let schmM = Map.lookup name env
  case schmM of
    Just schm -> do
      typeInferred <- instantiate schm
      typeDeclared <- instantiate scheme
      unify typeInferred typeDeclared
      pure . TypeEnv . Map.insert name schm $ env
    Nothing ->
      pure . TypeEnv . Map.insert name scheme $ env
accumulateDeclrs tenv@(TypeEnv env) (Binding (BindDefinition name expr)) = do
  let schmM = Map.lookup name env
  (subst, expTyp) <- ti tenv expr
  case schmM of
    Just schm -> do
      typ <- instantiate schm
      unify (apply subst expTyp) (apply subst typ)
      pure (TypeEnv env)
    Nothing ->
      pure . TypeEnv . Map.insert name (Scheme [] expTyp) $ env
accumulateDeclrs env _ = pure env

checkModule :: Module -> TIMonad TypeEnv
checkModule (Module _header modDeclrs) = do
  foldM accumulateDeclrs (TypeEnv Map.empty) modDeclrs

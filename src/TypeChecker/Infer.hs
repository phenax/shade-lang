module TypeChecker.Infer where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import TypeChecker.TIMonad
import Types

newTyVar :: String -> TIMonad Type
newTyVar prefix = do
  s <- get
  put s {tiSupply = tiSupply s + 1}
  return (TVariable (prefix ++ show (tiSupply s)))

instantiate :: Scheme -> TIMonad Type
instantiate (Scheme vars t) = do
  nvars <- mapM (\_ -> newTyVar "a") vars
  let s = Map.mapKeys getIdentName $ Map.fromList (zip vars nvars)
  return $ apply s t

unify :: Type -> Type -> TIMonad Subst
unify (TLambda l r) (TLambda l' r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s1 `composeSubst` s2)
unify (TVariable u) t = varBind u t
unify t (TVariable u) = varBind u t
unify TUnit TUnit = return Map.empty
unify TString TString = return Map.empty
unify TInt TInt = return Map.empty
unify TBool TBool = return Map.empty
unify t1 t2 =
  throwError $
    "types do not unify: "
      ++ show t1
      ++ " vs. "
      ++ show t2

varBind :: String -> Type -> TIMonad Subst
varBind u t
  | t == TVariable u = return Map.empty
  | u `Set.member` ftv t =
      throwError $
        "occurs check fails: "
          ++ u
          ++ " vs. "
          ++ show t
  | otherwise = return $ Map.singleton u t

ti :: TypeEnv -> Expr -> TIMonad (Subst, Type)
ti (TypeEnv env) (EVariable n) =
  case Map.lookup n env of
    Nothing -> throwError $ "unbound variable: " ++ getIdentName n
    Just tysch -> do
      t <- instantiate tysch
      return (Map.empty, t)
ti _ (ELiteral l) = case l of
  LString _ -> pure (Map.empty, TString)
  LInt _ -> pure (Map.empty, TInt)
  LBool _ -> pure (Map.empty, TBool)
  LUnit -> pure (Map.empty, TUnit)
ti env (ELambda n e) =
  do
    tv <- newTyVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv (env' `Map.union` Map.singleton n (Scheme [] tv))
    (s1, t1) <- ti env'' e
    return (s1, TLambda (apply s1 tv) t1)
ti env expr@(EApply fn arg) =
  do
    tv <- newTyVar "a"
    (s1, tfn) <- ti env fn
    (s2, targ) <- ti (apply s1 env) arg
    s3 <- unify (apply s2 tfn) (TLambda targ tv)
    return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
    `catchError` \e -> throwError $ e ++ "\n in " ++ show expr
ti env (EIfElse cond thenE elseE) = do
  (sCond, tCond) <- ti env cond
  unify TBool (apply sCond tCond)
    `catchError` (\e -> throwError $ e ++ "\n in " ++ show cond)

  (sThen, tThen) <- ti env thenE
  (_sElse, tElse) <- ti env elseE

  subst <- unify tThen (apply sThen tElse)
  pure (Map.empty, apply subst tThen)

typeInference :: TypeEnv -> Expr -> TIMonad Type
typeInference (TypeEnv env) e =
  do
    (s, t) <- ti (TypeEnv env) e
    return (apply s t)

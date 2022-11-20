module TypeChecker.Infer where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Types

instance TypeOperations Type where
  ftv (TVariable n) = Set.singleton n
  ftv TInt = Set.empty
  ftv TBool = Set.empty
  ftv (TLambda t1 t2) = ftv t1 `Set.union` ftv t2
  ftv _ = Set.empty

  apply s (TVariable n) = case Map.lookup n s of
    Nothing -> TVariable n
    Just t -> t
  apply s (TLambda t1 t2) = TLambda (apply s t1) (apply s t2)
  apply _s t = t

instance TypeOperations Scheme where
  ftv (Scheme vars t) = ftv t `Set.difference` Set.fromList (map getIdentName vars)
  apply s (Scheme vars t) = Scheme vars (apply (foldr (Map.delete . getIdentName) s vars) t)

instance TypeOperations a => TypeOperations [a] where
  apply s = map (apply s)
  ftv = foldr (Set.union . ftv) Set.empty

instance TypeOperations TypeEnv where
  ftv (TypeEnv env) = ftv (Map.elems env)
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

-- Figure out what this shit means
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme (map Identifier vars) t
  where
    vars = Set.toList (ftv t `Set.difference` ftv env)

data TIEnv = TIEnv {}

remove :: TypeEnv -> Identifier 'VariableName -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

data TIState = TIState {tiSupply :: Int}

type TI a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

runTI :: TI a -> IO (Either String a, TIState)
runTI t =
  do
    (res, st) <- runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
    return (res, st)
  where
    initTIEnv = TIEnv
    initTIState = TIState {tiSupply = 0}

newTyVar :: String -> TI Type
newTyVar prefix = do
  s <- get
  put s {tiSupply = tiSupply s + 1}
  return (TVariable (prefix ++ show (tiSupply s)))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
  nvars <- mapM (\_ -> newTyVar "a") vars
  let s = Map.mapKeys getIdentName $ Map.fromList (zip vars nvars)
  return $ apply s t

unify :: Type -> Type -> TI Subst
unify (TLambda l r) (TLambda l' r') = do
  s1 <- unify l l'
  s2 <- unify (apply s1 r) (apply s1 r')
  return (s1 `composeSubst` s2)
unify (TVariable u) t = varBind u t
unify t (TVariable u) = varBind u t
unify TInt TInt = return Map.empty
unify TBool TBool = return Map.empty
unify t1 t2 =
  throwError $
    "types do not unify: "
      ++ show t1
      ++ " vs. "
      ++ show t2

varBind :: String -> Type -> TI Subst
varBind u t
  | t == TVariable u = return Map.empty
  | u `Set.member` ftv t =
      throwError $
        "occurs check fails: "
          ++ u
          ++ " vs. "
          ++ show t
  | otherwise = return $ Map.singleton u t

ti :: TypeEnv -> Expr -> TI (Subst, Type)
ti (TypeEnv env) (EVariable n) =
  case Map.lookup n env of
    Nothing -> throwError $ "unbound variable: " ++ getIdentName n
    Just tysch -> do
      t <- instantiate tysch
      return (Map.empty, t)
-- ti _ (ELit l) = tiLit l
ti env (ELambda n e) =
  do
    tv <- newTyVar "a"
    let TypeEnv env' = remove env n
        env'' = TypeEnv (env' `Map.union` Map.singleton n (Scheme [] tv))
    (s1, t1) <- ti env'' e
    return (s1, TLambda (apply s1 tv) t1)
ti env exp@(EApply fn arg) =
  do
    tv <- newTyVar "a"
    (s1, tfn) <- ti env fn
    (s2, targ) <- ti (apply s1 env) arg
    s3 <- unify (apply s2 tfn) (TLambda targ tv)
    return (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
    `catchError` \e -> throwError $ e ++ "\n in " ++ show exp
ti env exp = undefined

typeInference :: TypeEnv -> Expr -> TI Type
typeInference (TypeEnv env) e =
  do
    (s, t) <- ti (TypeEnv env) e
    return (apply s t)

module TypeChecker.TIMonad where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set
import Types

newtype TIState = TIState {tiSupply :: Int}

type TIMonad a = ExceptT String (ReaderT TIEnv (StateT TIState IO)) a

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
  ftv (Scheme vars t) = ftv t `Set.difference` Set.fromList vars
  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance TypeOperations a => TypeOperations [a] where
  apply s = map (apply s)
  ftv = foldr (Set.union . ftv) Set.empty

instance TypeOperations TypeEnv where
  ftv (TypeEnv env) = ftv (Map.elems env)
  apply s (TypeEnv env) = TypeEnv (Map.map (apply s) env)

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
  where
    vars = Set.toList (ftv t `Set.difference` ftv env)

data TIEnv = TIEnv {}

remove :: TypeEnv -> Identifier 'VariableName -> TypeEnv
remove (TypeEnv env) var = TypeEnv (Map.delete var env)

runTI :: TIMonad a -> IO (Either String a, TIState)
runTI t =
  do
    (res, st) <- runStateT (runReaderT (runExceptT t) initTIEnv) initTIState
    return (res, st)
  where
    initTIEnv = TIEnv
    initTIState = TIState {tiSupply = 0}

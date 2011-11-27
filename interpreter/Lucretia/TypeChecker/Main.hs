module Lucretia.TypeChecker.Main(runCheck, checkProg) where

import Data.Map(Map)
import qualified Data.Map as Map

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

import Lucretia.TypeChecker.Syntax
import Lucretia.TypeChecker.Types


emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Name -> Type -> Env -> Env
extendEnv x t env = Map.insert x t env

initState :: CheckState
initState = CheckState [] [1..]
  
type CM a = StateT CheckState (ErrorT String Identity) a
runCM :: CM a -> CheckState -> Either String (a,CheckState)
runCM m st = runIdentity $ runErrorT $ runStateT m st


runCheck :: Exp -> Either String (Type, CheckState)
runCheck e = runCM (findType emptyEnv e) initState 

checkProg :: Program -> Bool
checkProg defs = isRight $ runCheck (ELets defs ENew) --ENew could be anything

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left  _) = False

freshInt :: CM Int
freshInt = do
  cst <- get
  let ints = cstFresh cst
  put $ cst { cstFresh = tail ints }
  return $ head ints
  
freshName :: String -> CM String
freshName s = do 
  i <- freshInt
  return $ s ++ show i

addConstraint :: Constraint -> CM ()
addConstraint c = do
  cst <- get
  put $ cst { cstCons = c:cstCons cst }
  
freshTVar = freshName "X"

emptyRecType :: Type
emptyRecType = TRec Map.empty

envType :: Env -> Name -> CM Type
envType env x = case Map.lookup x env of
  Nothing -> throwError $ "Unknown var "++x
  Just t -> return t
  
-- TODO tests
findType :: Env -> Exp -> CM Type
findType env (EVar x) = envType env x 
findType env ENew = do
  t <- freshTVar
  addConstraint $ Constraint (t,emptyRecType)
  return $ TVar t
findType env (ELet x e1 e0) = do  
  t1 <- findType env e1
  let env' = extendEnv x t1 env
  findType env' e0
findType env (ELets [] e0) = findType env e0
findType env (ELets ((x,e):ds) e0) = findType env (ELet x e (ELets ds e0))
findType env (ESet x a e) = do
  TVar tX <- envType env x
  t2 <- findType env e
  addConstraint $ Constraint (tX,oneFieldRec a t2)
  return (TVar tX)

--TODO uncomment, make run
{-
findType env (EGet x a) = do
  u <- findConstraint a
  --TODO map constraints

  guard $ doesNotHaveBottom u
  -}
  
oneFieldRec :: Name -> Type -> Type
oneFieldRec a t = TRec $ Map.fromList [(a,t)]

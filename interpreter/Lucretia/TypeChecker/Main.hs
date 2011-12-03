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
initState = CheckState Map.empty [1..]
  
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




type ModifyConstraints = Constraints -> Constraints

modifyConstraints :: ModifyConstraints -> CM()
modifyConstraints f = modify $ \cst -> cst { cstCons = f (cstCons cst) }

createEmptyConstraint :: Name -> CM ()
createEmptyConstraint = modifyConstraints . createEmptyConstraint'

createEmptyConstraint' :: Name -> ModifyConstraints
createEmptyConstraint' record = Map.insert record emptyRecType

extendRecordConstraint :: Name -> Name -> Type -> CM()
extendRecordConstraint r a t = modifyConstraints $ extendRecordConstraint' r a t

extendRecordConstraint' :: Name -> Name -> Type -> ModifyConstraints
extendRecordConstraint' recordName attribute t = Map.adjust (addAttribute attribute t) recordName

addAttribute :: Name -> Type -> RecType -> RecType
addAttribute = Map.insert




freshTVar = freshName "X"

emptyRecType :: RecType
emptyRecType = Map.empty

envType :: Env -> Name -> CM Type
envType env x = case Map.lookup x env of
  Nothing -> throwError $ "Unknown var "++x
  Just t -> return t
  




findType :: Env -> Exp -> CM Type
findType env (EVar x) = envType env x 
findType env (EInt _) = return TInt
findType env EBoolTrue = return TBool
findType env EBoolFalse = return TBool
findType env ENew = do
  t <- freshTVar
  createEmptyConstraint t
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
  extendRecordConstraint tX a t2
  return (TVar tX)

{-
-- TODO uncomment, make run
findType env (EGet x a) = do
  tX <- envType env x
  case tX of
    TRec t     -> do
      guard $ doesNotHaveBottom u

doesNotHaveBottom :: Type -> Bool
doesNotHaveBottom TInt = True
doesNotHaveBottom TBool = True
doesNotHaveBottom TVar _ = True
doesNotHaveBottom TRec _ = True
doesNotHaveBottom TOr t1 t2 = doesNotHaveBottom t1 && doesNotHaveBottom t2
doesNotHaveBottom TFieldUndefined = False
    --TOr ts -> TOr mapMonad ts
      

-- TODO uncomment, make run
    otherwise -> throwError $ x ++ " should be of an object type, but is " ++ show tX ++ "\n" ++ "  In the expression " ++ show (EGet x a) --showPretty


  u <- getConstraintFor x
  --zamiana na mapę
  --TODO map constraints



envType :: Env -> Name -> CM Type
envType env x = case Map.lookup x env of
  Nothing -> throwError $ "Unknown var "++x
  Just t -> return t
  
-}

findType env (EIf eIf eThen eElse) = do
  TBool <- findType env eIf
  stateBeforeBody <- get
  
  put stateBeforeBody
  tThen <- findType env eThen
  stateAfterThen <- get
  
  put stateBeforeBody
  tElse <- findType env eElse
  stateAfterElse <- get

  put $ mergeStates stateAfterThen stateAfterElse
  return $ mergeTypes tThen tElse

mergeStates :: CheckState -> CheckState -> CheckState
mergeStates cst1 cst2 = CheckState (mergeCons (cstCons cst1) (cstCons cst2))
                                   (mergeFresh (cstFresh cst1) (cstFresh cst2))

mergeCons :: Constraints -> Constraints -> Constraints
mergeCons = Map.unionWith mergeRecTypes

mergeRecTypes :: RecType -> RecType -> RecType
mergeRecTypes r1 r2 = 
  Map.union intersection rest
    where
    intersection = Map.intersectionWith mergeTypes r1 r2
    rest = Map.map canBeUndefined r1_xor_r2
      where canBeUndefined (TOr ts) = TOr (TFieldUndefined:ts)
            canBeUndefined t = TOr [TFieldUndefined, t] 
    r1_xor_r2 = Map.union (Map.difference r1 r2) (Map.difference r2 r1)

mergeTypes :: Type -> Type -> Type
mergeTypes t1 t2
  | t1 == t2  = t1
  | otherwise = mergeInequalTypes t1 t2
  where
  mergeInequalTypes :: Type -> Type -> Type
  mergeInequalTypes (TOr t1) (TOr t2) = TOr $ t1 ++ t2
  mergeInequalTypes t1 (TOr t2) = 
    if t1 `elem` t2
      then TOr t2
      else TOr $ t1:t2
  mergeInequalTypes (TOr t1) t2 =
    if t2 `elem` t1
      then TOr t1
      else TOr $ t2:t1
  mergeInequalTypes t1 t2 = TOr [t1, t2]

mergeFresh :: [Int] -> [Int] -> [Int]
mergeFresh (fresh1:_) (fresh2:_) = [(max fresh1 fresh2)..]
  
oneFieldTRec :: Name -> Type -> RecType
oneFieldTRec a t = Map.fromList [(a,t)]

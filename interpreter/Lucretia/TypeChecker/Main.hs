module Lucretia.TypeChecker.Main(runCheck, checkProg) where

import Data.Map(Map)
import qualified Data.Foldable as Foldable
import qualified Data.Map as Map

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

import DebugUtils(traceShowId, traceShowIdHl)

import Lucretia.TypeChecker.Definitions(Name, Param)
import Lucretia.TypeChecker.Types
import Lucretia.TypeChecker.Syntax


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
runCheck e = runCM (findTypeCleanConstraints emptyEnv e) initState 

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

modifyConstraints :: ModifyConstraints -> CM ()
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
  


findTypeCleanConstraints :: Env -> Exp -> CM Type
findTypeCleanConstraints env e = do
  returnType <- findType env e
  modifyConstraints $ cleanConstraints env returnType
  return returnType

filterTVars :: [Type] -> [Name]
filterTVars ts = map (\(TVar n) -> n) $ filter isTVar $ ts
  where
    isTVar (TVar _) = True
    isTVar _ = False
    --TODO refactor: is there a language construct for functions like isTVar?

cleanConstraints :: Env -> Type -> Constraints -> Constraints
cleanConstraints env returnType inputCs = foldl addConstraintsForName Map.empty tVarsNeededInEnv
  where
  tVarsNeededInEnv :: [Name]
  tVarsNeededInEnv = filterTVars $ returnType:(Map.elems env)

  addConstraintsForName :: Constraints -> Name -> Constraints
  addConstraintsForName neededCsAcc neededTVarName
    | neededTVarName `Map.member` neededCsAcc = neededCsAcc
    | otherwise = foldl addConstraintsForName neededCsAccIncreased tVarsNeededInNeededTVar
    where
    neededTVarType = inputCs Map.! neededTVarName 
    tVarsNeededInNeededTVar = filterTVars $ Map.elems neededTVarType
    neededCsAccIncreased = Map.insert neededTVarName neededTVarType neededCsAcc



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
  t1 <- findTypeCleanConstraints env e1
  let env' = extendEnv x t1 env
  findTypeCleanConstraints env' e0
findType env (ELets [] e0) = findTypeCleanConstraints env e0
findType env (ELets ((x,e):ds) e0) = findTypeCleanConstraints env (ELet x e (ELets ds e0))
findType env (ESet x a e) = do
  TVar tX <- envType env x
  t2 <- findTypeCleanConstraints env e
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
  TBool <- findTypeCleanConstraints env eIf
  stateBeforeBody <- get
  
  put stateBeforeBody
  tThen <- findTypeCleanConstraints env eThen
  stateAfterThen <- get
  
  put stateBeforeBody
  tElse <- findTypeCleanConstraints env eElse
  stateAfterElse <- get

  put $ mergeStates stateAfterThen stateAfterElse
  return $ mergeTypes tThen tElse

findType env (EFunc (Func xParams tFunc eBody)) = do
  let TFunc expectedConstraintsBefore tParams expectedU expectedConstraintsAfter = tFunc
  (length xParams == length tParams) `orFail` "Number of arguments and number of their types doesn't match"
  let params = zip xParams tParams
  let extendedEnv = foldl (\envAccumulator (eXi, tXi) -> Map.insert eXi tXi envAccumulator) env params
  constraintsBeforeBody <- getConstraints

  putConstraints expectedConstraintsBefore
  u <- findTypeCleanConstraints extendedEnv eBody
  actualConstraintsAfter <- getConstraints
  --(expectedU, expectedConstraintsAfter) `areIsomorphic` (u, actualConstraintsAfter)
  (TypeIso expectedU expectedConstraintsAfter == TypeIso u actualConstraintsAfter) `orFail` ("Type and associated constraints after type-checking method body: " ++ show u ++ ", " ++ showConstraints actualConstraintsAfter ++ " are not the same as declared in the signature: " ++ show expectedU ++ ", " ++ showConstraints expectedConstraintsAfter ++ ".")
  --TODO maybe this:
  --(actualConstraintsAfter `areAtLeastThatStrongAs` expectedConstraintsAfter) `orFail` ("Returned value should match at least all the constraints that are declared in the signature of function " ++ funcName ++ ".")

  putConstraints constraintsBeforeBody

  return tFunc --TODO: test eFuncWithUnnecessaryConstraints, tFunc { constraintsAfter = cleanConstraints (Map.fromList [("_", expectedU)] (constraintsAfter tFunc) }

findType env (ECall eFunc eParams) = do
  let funcName = findName env eFunc

  tFunc <- findTypeCleanConstraints env eFunc
  let TFunc expectedConstraintsBefore tParams tBody expectedConstraintsAfter = tFunc
  (length eParams == length tParams) `orFail` ("Function " ++ funcName ++ " is applied to " ++ show (length eParams) ++ " parameters, but " ++ show (length tParams) ++ " parameters should be provided.")
  
  tParamsActual <- mapM (findTypeCleanConstraints env) eParams
  (tParams == tParamsActual) `orFail` ("Types of parameters should be " ++ show tParams ++ " but are " ++ show tParamsActual ++ ".")

  actualConstraintsBeforeBody <- getConstraints
  (expectedConstraintsBefore `constraintsAreWeakerOrEqualTo` actualConstraintsBeforeBody) `orFail` ("Constraints before calling function " ++ funcName ++ ": " ++ showConstraints actualConstraintsBeforeBody ++ " are not that strong as pre-constraints in the function definition: " ++ showConstraints expectedConstraintsBefore ++ ".")

  putConstraints $ actualConstraintsBeforeBody `merge` expectedConstraintsAfter

  return tBody

  where

  constraintsAreWeakerOrEqualTo :: Constraints -> Constraints -> Bool
  constraintsAreWeakerOrEqualTo = Map.isSubmapOfBy recordIsSmallerOrEqualTo
  
  recordIsSmallerOrEqualTo :: RecType -> RecType -> Bool
  recordIsSmallerOrEqualTo = Map.isSubmapOf

  merge :: Constraints -> Constraints -> Constraints
  merge = Map.unionWith seq

findName :: Env -> Exp -> Name
findName env (EVar x) = x
findName env _ = "(anonymous)"
  
orFail :: MonadError e m => Bool -> e -> m ()
orFail cond errorMsg =
  unless cond $ throwError errorMsg

getConstraints :: CM Constraints
getConstraints = do
  state <- get
  return $ cstCons state

putConstraints :: Constraints -> CM ()
putConstraints constraints = do
  state <- get
  put $ state { cstCons = constraints }

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
  





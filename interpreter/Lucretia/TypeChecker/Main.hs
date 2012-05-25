-----------------------------------------------------------------------------
-- |
-- Module      :  Lucretia.TypeChecker.Main
-- Copyright   :  (c) Marcin Benke     2011, 2012
--                (c) Michał Oniszczuk 2011, 2012
--
-- Maintainer  :  mo262537@students.mimuw.edu.pl
--
-- How the white paper structure is reflected in the code:
--
-- /1. Introduction/
--
-- /2. Model language and its semantics/
--
-- * Definiton 2.1 (Types) @->@ "Lucretia.TypeChecker.Types"
--
-- * Figure 1: Syntax of lambda_M @->@ "Lucretia.Syntax"
--
-- * Figure 2: Semantic rules of lambda_M @->@ "Lucretia.Interpreter"
--
-- /3. The type system/ @->@ "Lucretia.TypeChecker.Main" (this module)
--
-- /4. Properties of the type system/
-- 
-- /5. Related work/
-- 
-- /6. Conclusions and future work/
--
-----------------------------------------------------------------------------
--module Lucretia.TypeChecker.Main (runCheck, checkProg) where
module Lucretia.TypeChecker.Main where

import qualified Data.Foldable as Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

import DebugUtils (traceShowId, traceShowIdHl)

import OrFail (orFail, orFailE)

import Lucretia.TypeChecker.Definitions (Name, Param)
import Lucretia.TypeChecker.Types
import Lucretia.TypeChecker.Syntax
import Lucretia.TypeChecker.IsomorphicModuloNames (iso)


-- * Type checker rules (/3. The type system/ in wp)

findType :: Env -> Exp -> CM Type

-- | Record update (update-old)
findType env (ESet x a e) = do
  TVar tX <- envType env x
  t2 <- findTypeCleanConstraints env e
  extendRecordConstraint tX a t2
  return (TVar tX)

-- | Record access
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

-- | Conditional instruction (if)
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

-- | Function definition (fdecl)
findType env (EFunc (Func xParams tFunc eBody)) = do
  let TFunc expectedConstraintsBefore tParams expectedU expectedConstraintsAfter = tFunc
  (length xParams == length tParams) `orFail` "Number of arguments and number of their types do not match"
  let params = zip xParams tParams
  let extendedEnv = foldl (\envAccumulator (eXi, tXi) -> Map.insert eXi tXi envAccumulator) env params
  constraintsBeforeBody <- getConstraints

  putConstraints expectedConstraintsBefore
  u <- findTypeCleanConstraints extendedEnv eBody
  actualConstraintsAfter <- getConstraints
  iso (expectedU, expectedConstraintsAfter) (u, actualConstraintsAfter) `orFailE` ("Type and associated constraints after type-checking method body: " ++ show u ++ ", " ++ showConstraints actualConstraintsAfter ++ " are not the same as declared in the signature: " ++ show expectedU ++ ", " ++ showConstraints expectedConstraintsAfter ++ ".\n")
  --TODO maybe this:
  --(actualConstraintsAfter `areAtLeastThatStrongAs` expectedConstraintsAfter) `orFail` ("Returned value should match at least all the constraints that are declared in the signature of function " ++ funcName ++ ".")

  putConstraints constraintsBeforeBody

  return tFunc --TODO: test eFuncWithUnnecessaryConstraints, tFunc { constraintsAfter = cleanConstraints (Map.fromList [("_", expectedU)] (constraintsAfter tFunc) }

-- | Function call (fapp)
findType env (ECall eFunc eParams) = do
  let funcName = findName env eFunc

  tFunc <- findTypeCleanConstraints env eFunc
  let TFunc expectedConstraintsBefore tParams tBody expectedConstraintsAfter = tFunc
  (length eParams == length tParams) `orFail` ("Function " ++ funcName ++ " is applied to " ++ show (length eParams) ++ " parameters, but " ++ show (length tParams) ++ " parameters should be provided.\n")
  
  tParamsActual <- mapM (findTypeCleanConstraints env) eParams
  (tParams == tParamsActual) `orFail` ("Types of parameters should be " ++ show tParams ++ " but are " ++ show tParamsActual ++ ".\n")

  actualConstraintsBeforeBody <- getConstraints
  (expectedConstraintsBefore `constraintsAreWeakerOrEqualTo` actualConstraintsBeforeBody) `orFail` ("Constraints before calling function " ++ funcName ++ ": " ++ showConstraints actualConstraintsBeforeBody ++ " are not that strong as pre-constraints in the function definition: " ++ showConstraints expectedConstraintsBefore ++ ".\n")

  putConstraints $ actualConstraintsBeforeBody `merge` expectedConstraintsAfter

  return tBody

  where

  constraintsAreWeakerOrEqualTo :: Constraints -> Constraints -> Bool
  constraintsAreWeakerOrEqualTo = Map.isSubmapOfBy recordIsSmallerOrEqualTo
  
  recordIsSmallerOrEqualTo :: RecType -> RecType -> Bool
  recordIsSmallerOrEqualTo = Map.isSubmapOf

  merge :: Constraints -> Constraints -> Constraints
  merge = Map.unionWith seq

-- | Control flow with break instructions (label)
-- TODO
-- | Control flow with break instructions (break)
-- TODO

-- | Let-expression (let)
findType env (ELet x e1 e0) = do  
  t1 <- findTypeCleanConstraints env e1
  let env' = extendEnv x t1 env
  findTypeCleanConstraints env' e0
findType env (ELets [] e0) = findTypeCleanConstraints env e0
findType env (ELets ((x,e):ds) e0) = findTypeCleanConstraints env (ELet x e (ELets ds e0))

-- | Object creation (new)
findType env ENew = do
  t <- freshTVar
  createEmptyConstraint t
  return $ TVar t

-- | Other rules, not mentioned in the white paper
findType env (EVar x) = envType env x 
findType env (EInt _) = return TInt
findType env EBoolTrue = return TBool
findType env EBoolFalse = return TBool

envType :: Env -> Name -> CM Type
envType env x = case Map.lookup x env of
  Nothing -> throwError $ "Unknown var "++x
  Just t -> return t

-- ** Helper functions

findName :: Env -> Exp -> Name
findName env (EVar x) = x
findName env _ = "(anonymous)"
  
-- ** Merging functions (@Bishop@ relation in /Conditional instruction/ in wp)

-- | Merge CheckStates. Not in wp, see 'mergeFresh' below for explanation.
mergeStates :: CheckState -> CheckState -> CheckState
mergeStates cst1 cst2 = CheckState (mergeCons (cstCons cst1) (cstCons cst2))
                                   (mergeFresh (cstFresh cst1) (cstFresh cst2))

-- | Merge state (number of next fresh variable in this implementation) of
-- fresh variable sources.
--
-- This is not mentioned in wp (??) but this also must be merged.
mergeFresh :: [Int] -> [Int] -> [Int]
mergeFresh (fresh1:_) (fresh2:_) = [(max fresh1 fresh2)..]

-- | Merge Constraints, @Bishop@ relation in wp.
mergeCons :: Constraints -> Constraints -> Constraints
mergeCons = Map.unionWith mergeRecTypes

-- | Merge RecTypes, @Bishop@ relation in wp.
mergeRecTypes :: RecType -> RecType -> RecType
mergeRecTypes r1 r2 = 
  Map.union intersection rest
    where
    intersection = Map.intersectionWith mergeTypes r1 r2
    rest = Map.map canBeUndefined r1_xor_r2
      where canBeUndefined (TOr ts) = TOr $ Set.insert TFieldUndefined ts
            canBeUndefined t = TOr $ Set.fromList [TFieldUndefined, t] 
    r1_xor_r2 = Map.union (Map.difference r1 r2) (Map.difference r2 r1)

-- | Merge Types, @Bishop@ relation in wp.
mergeTypes :: Type -> Type -> Type
mergeTypes t1 t2
  | t1 == t2  = t1
  | otherwise = mergeInequalTypes t1 t2
  where
  mergeInequalTypes :: Type -> Type -> Type
  mergeInequalTypes (TOr t1) (TOr t2) = TOr $ Set.union t1 t2
  mergeInequalTypes t1 (TOr t2) = TOr $ Set.insert t1 t2
  mergeInequalTypes (TOr t1) t2 = TOr $ Set.insert t2 t1
  mergeInequalTypes t1 t2 = TOr $ Set.fromList [t1, t2]

-- * Garbage collection of unnecessary Constraints (not in wp?)

findTypeCleanConstraints :: Env -> Exp -> CM Type
findTypeCleanConstraints env e = do
  returnType <- findType env e
  modifyConstraints $ cleanConstraints env returnType
  return returnType

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

  filterTVars :: [Type] -> [Name]
  filterTVars ts = map (\(TVar n) -> n) $ filter isTVar $ ts
    where
      isTVar (TVar _) = True
      isTVar _ = False
      --TODO refactor: is there a language construct for functions like isTVar?

-- * Type checker monad

type CM a = StateT CheckState (ErrorT String Identity) a

runCM :: CM a -> CheckState -> Either String (a,CheckState)
runCM m st = runIdentity $ runErrorT $ runStateT m st

runCheck :: Exp -> Either String (Type, CheckState)
runCheck e = runCM (findTypeCleanConstraints emptyEnv e) initState 

checkProg :: Program -> Bool
checkProg defs = isRight $ runCheck (ELets defs ENew) -- ENew could be anything

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left  _) = False

-- ** Fresh variables (… in wp)

-- | Get fresh Type Variable Name
freshTVar = freshName "X"

-- | Get fresh Type Variable Name with given prefix
freshName :: String -- ^ prefix
          -> CM String
freshName prefix = do 
  i <- freshInt
  return $ prefix ++ show i

-- | Get next fresh Int
freshInt :: CM Int
freshInt = do
  cst <- get
  let ints = cstFresh cst
  put $ cst { cstFresh = tail ints }
  return $ head ints
  
-- ** Constraints

getConstraints :: CM Constraints
getConstraints = do
  state <- get
  return $ cstCons state

putConstraints :: Constraints -> CM ()
putConstraints constraints = do
  state <- get
  put $ state { cstCons = constraints }

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


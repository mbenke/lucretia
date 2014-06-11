-----------------------------------------------------------------------------
-- |
-- Module      :  Lucretia.TypeChecker.Main
-- Copyright   :  (c) Marcin Benke     2011 - 2014
--                (c) Michał Oniszczuk 2011 - 2014
-- Maintainer  :  michal.oniszczuk@gmail.com
--
-- How the white paper structure is reflected in the code:
--
-- /1. Introduction/
--
-- /2. Model language and its semantics/
--
-- * Definiton 2.1 (Language.Types) @->@ "Lucretia.Language.Types"
--
-- * Figure 1: Syntax of lambda_M @->@ "Lucretia.Language.Syntax"
--
-- * Figure 2: Semantic rules of lambda_M @->@ "Lucretia.Interpreter"
--
-- /3. The type system/ @->@ "Lucretia.TypeChecker" (this module)
--
-- /4. Properties of the type system/
-- 
-- /5. Related work/
-- 
-- /6. Conclusions and future work/
--
-----------------------------------------------------------------------------

--module Lucretia.TypeChecker (runCheck, checkProg) where
module Lucretia.TypeChecker where

import Prelude hiding (all)

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Foldable (all)

import Data.Lens

import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Identity

import Util.Debug (traceShowId, traceShowIdHl)

import Util.OrFail (orFail)

import Lucretia.Language.Definitions (Var, Field, TVar)
import Lucretia.Language.Types
import Lucretia.Language.Syntax
import Lucretia.TypeChecker.MonomorphicModuloNames (weakerOrEqualTo, monoRename)

accessRecord :: Env -> Var -> CM Env
accessRecord env x = getTVar env x >>= access . record -- TODO OPT RTR using lenses with errors library

-- * Type checker rules (/3. The type system/ in wp)

findType :: Env -> Exp -> CM Type

-- Record update (update-old)
findType env (ESet x a e) = do
  tX <- getTVar env x
  t2 <- findTypeCleanConstraints env e
  field a tX ~= Just t2
  return $ TVar tX

-- Record access (access)
findType env (EGet x a) = do
  v <- getTVar env x
  u <- accessField a v
  doesNotHaveBottom u `orFail` (x++"."++a++" may be undefined.")
  return u
  where
  accessField :: Field -> TVar -> CM Type
  accessField a v = do
    contains a v
    Just u <- access $ field a v
    return u
  doesNotHaveBottom :: Type -> Bool
  doesNotHaveBottom TFieldUndefined = False
  doesNotHaveBottom (TOr t) = all doesNotHaveBottom t
  doesNotHaveBottom _ = True

-- Conditional instruction (if)
findType env i@(EIf eIf eThen eElse) = do
  TBool <- findTypeCleanConstraints env eIf
  stateBeforeBody <- get
  
  tThen <- findTypeCleanConstraints env eThen
  stateAfterThen <- get
  
  put stateBeforeBody

  tElse <- findTypeCleanConstraints env eElse
  stateAfterElse <- get

  (tThen == tElse) `orFail` ("Type after then: "++show tThen++" does not match type after else: "++show tElse++". In "++show i)

  put $ mergeStates stateAfterThen stateAfterElse
  return tThen

-- Object structure introspection (ifhasattr)
findType env i@(EIfHasAttr x a eThen eElse) = do
  stateBeforeBody <- get
  
  assumeContains a x
  tThen <- findTypeCleanConstraints env eThen
  stateAfterThen <- get
  
  put stateBeforeBody

  tElse <- findTypeCleanConstraints env eElse
  stateAfterElse <- get

  (tThen == tElse) `orFail` ("Type after then: "++show tThen++" does not match type after else: "++show tElse++". In "++show i)

  put $ mergeStates stateAfterThen stateAfterElse
  return tThen
    where
    assumeContains :: Field -> Var -> CM (Maybe Type)
    assumeContains a x = do
      v <- getTVar env x
      contains a v
      field a v %= liftM removeBottom

    removeBottom :: Type -- ^ Note that TFieldUndefined can be here only as
                         -- a part (one alternative) of TOr
		 -> Type
    removeBottom (TOr t) = mkTOr $ Set.filter (/= TFieldUndefined) t
    removeBottom t = t

    -- | Creates TOr type from Set of Language.Types.
    -- Takes care of case when the set is a singleton.
    mkTOr :: Set.Set Type -> Type
    mkTOr s
      | Set.size s == 1 = (head . Set.toList) s
      | otherwise       = TOr s

      -- co jesli x.a niezdef? A: error musi byc chociaz szansa ze x.a jest
      --jak mam modelowac wartosci typu int z Pythona? jako rekord?

-- Variable and location access (v-access)
findType env (EVar x) = getType env x 

-- Variable and location access (l-access)
-- TODO

-- Function definition (fdecl)
--
-- ed- stands for e(xpecte)d (by signature)
-- al- stands for a(ctua)l (infered from expressions)
-- -Pre-  stands for  Pre(contidions)
-- -Post- stands for Post(contidions)
-- -T  stands for Type
-- -Cs stands for Constraints
findType env (EFunDecl edPreVars eBody) = do
  return TNone

-- Function call (fapp)
findType env (EFunCall funcE alPreEs) = do
  let funcName = findName env funcE

  TFunc edPreCs edEnv edPreTs edPostT edPostCs <- findTypeCleanConstraints env funcE

  (length alPreEs == length edPreTs) `orFail` ("Function "++funcName++" is applied to "++show (length alPreEs)++" parameters, but "++show (length edPreTs)++" parameters should be provided.\n")
  
  alPreTs <- mapM (findTypeCleanConstraints env) alPreEs
  alPreCs <- access constraints
  mono <- (edPreTs, edPreCs) `weakerOrEqualTo` (alPreTs, alPreCs)

  constraints ~= alPreCs `mergeUpdate` monoRename mono edPostCs
  return edPostT

-- Let-expression (let)
findType env (ELet x e1 e0) = do  
  t1 <- findTypeCleanConstraints env e1
  let env' = extendEnv x t1 env
  findTypeCleanConstraints env' e0

-- Object creation (new)
findType env ENew = do
  v <- freshTVar
  record v ~= emptyRecType
  return $ TVar v

-- Other rules, not mentioned in the white paper
findType env (EInt _) = return TInt
findType env (EBool _) = return TBool
findType env (EStr _) = return TStr
findType env ENone = return TNone
findType env (EAdd e e') = do
  t  <- findType env e
  t' <- findType env e'
  (t /= TNone && t' /= TNone)
    `orFail` ("TypeError: unsupported operand type(s) for +: "
             ++show t++" and "++show t')
  return TInt

-- ** Type information update (Definition 3.4 in wp)

mergeUpdate :: Constraints -> Constraints -> Constraints
mergeUpdate = Map.unionWith mergeUpdateRecord
  where
  mergeUpdateRecord = Map.unionWith $ flip const

-- ** Helper functions

getTVar :: Env -> TVar -> CM TVar
getTVar env x = unpackTVar =<< getType env x

unpackTVar :: Type -> CM TVar
unpackTVar (TVar tVar) = return tVar
unpackTVar t = throwError $ "Variable type mismatch: expected record type, but got "++show t++"."

getType :: Env -> Var -> CM Type
getType env x = case Map.lookup x env of
  Nothing -> throwError $ "Unknown variable "++x++"."
  Just t -> return t

findName :: Env -> Exp -> Var
findName env (EVar x) = x
findName env _ = "(anonymous)"

-- | Could have been refactored using Lenses supporting MonadError
--
-- TODO OPT: write a library for Lenses using MonadError
--
contains :: Field -> TVar -> CM ()
contains a tvar = do
  aValue <- access $ field a tvar
  rec <- access $ record tvar
  case aValue of
	Nothing -> throwError $ "Record "++showRec rec++" does not contain field "++a
	_ -> return ()

  
-- ** Merging functions (@Bishop@ relation in /Conditional instruction/ in wp)

-- | Merge CheckStates. Not in wp, see 'mergeFresh' below for explanation.
mergeStates :: CheckState -> CheckState -> CheckState
mergeStates cst1 cst2 = CheckState (mergeCons (_constraints cst1) (_constraints cst2))
                                   (mergeFresh (_freshInts cst1) (_freshInts cst2))

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
mergeRecTypes :: Rec -> Rec -> Rec
mergeRecTypes r1 r2 = 
  intersection `Map.union` rest
    where
    intersection = Map.intersectionWith mergeTypes r1 r2
    rest = Map.map canBeUndefined r1_xor_r2
      where canBeUndefined (TOr ts) = TOr $ Set.insert TFieldUndefined ts
            canBeUndefined t = TOr $ Set.fromList [TFieldUndefined, t] 
    r1_xor_r2 = Map.difference r1 r2 `Map.union` Map.difference r2 r1

-- | Merge Language.Types, @Bishop@ relation in wp.
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


findTypeCleanConstraints env e = do
  returnType <- findType env e
  constraints %= cleanConstraints env returnType
  return returnType

cleanConstraints :: Env -> Type -> Constraints -> Constraints
cleanConstraints env returnType inputCs = foldl addCsForName Map.empty tVarsNeededInEnv
  where
  tVarsNeededInEnv :: [TVar]
  tVarsNeededInEnv = filterTVars $ returnType : Map.elems env

  addCsForName :: Constraints -> TVar -> Constraints
  addCsForName neededCsAcc neededTVarName
    | neededTVarName `Map.member` neededCsAcc = neededCsAcc
    | otherwise = foldl addCsForName neededCsAccIncreased tVarsNeededInNeededTVar
    where
    neededTVarType = inputCs Map.! neededTVarName 
    tVarsNeededInNeededTVar = filterTVars $ Map.elems neededTVarType
    neededCsAccIncreased = Map.insert neededTVarName neededTVarType neededCsAcc

  filterTVars :: [Type] -> [TVar]
  filterTVars = map (\(TVar n) -> n) . filter isTVar
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

-- ** Fresh variables (… in wp)

-- | Get fresh 'TVar'
freshTVar :: CM String
freshTVar = freshName "X"

-- | Get fresh variable name with given prefix
freshName :: String -- ^ prefix
          -> CM String
freshName prefix = do 
  i <- freshInt
  return $ prefix++show i

-- | Get next fresh Int
freshInt :: CM Int
freshInt = do
  ints <- access freshInts
  freshInts ~= tail ints
  return $ head ints


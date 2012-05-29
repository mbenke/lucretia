{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Lucretia.Types where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Lucretia.Definitions (Var, TVar)

-- * Types (/Defition 2.1 (Types)/ in wp)

-- | @t_b@ /and/ @t@ in wp
data Type
  = TInt -- ^ @c@ in wp
  | TBool -- ^ @c@ in wp
  | TVar TVar -- ^ @X@ in wp
  | TRec Rec -- ^ @t_r@ in wp
  | TOr (Set Type) -- ^ @t_b,1 v t_b,2@ in wp
  | TFieldUndefined -- ^ @_|_@ in wp
  | TFunc Constraints [Type] Type Constraints -- ^ @[t1, …, tn; Psi_1] => [tn+1; \Psi_2]@ in wp
  deriving (Eq, Ord)

-- * Record Type (@t_r = {l : t} | {}@ in wp)

-- | Record (models an object). A mapping from field names to Types.
--
-- List of pairs @l : t@ in wp.
--
-- It is the same type as 'Env'
type Rec = Map Var Type

oneFieldTRec :: Var -> Type -> Rec
oneFieldTRec a t = Map.fromList [(a,t)]

emptyRecType :: Rec
emptyRecType = Map.empty


-- * Constraints (@Psi@ in wp)

-- | A mapping from Type Variable Names to Record Types.
--
-- List of pairs @X <# t_r@ in wp.
type Constraints = Map TVar Rec

-- * Environment: Variable Names to Types (Sigma in wp)

-- | A mapping from Variable Names to Types.
--
-- It is the same type as 'Env'
type Env = Map Var Type

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Var -> Type -> Env -> Env
extendEnv x t env = Map.insert x t env

-- * Locations to Type Variable Names (Gamma in wp)

data CheckState = CheckState { 
  cstCons :: Constraints,
  cstFresh :: [Int]
}
  deriving Eq

instance Show CheckState where
  show cst = showConstraints $ cstCons cst
  
initState :: CheckState
initState = CheckState Map.empty [1..]
  
-- * Show instance

showConstraints cs = concat ["[",showFields fields,"]"] where
  fields = Map.toList cs
  showFields fields = intercalate ", " (map showField fields)
  showField (l,t) = concat [l," < ", showRec t]

showRec :: Rec -> String 
showRec r = concat ["{",showFields fields,"}"] where
  fields = Map.toList r 
  showFields fields = intercalate ", " (map showField fields)
  showField (l,t) = concat [l,":",show t]
  
instance Show Type where
  show TInt = "int"
  show TBool = "bool"
  show (TVar v) = v
  show (TRec r) = showRec r
  show (TOr ts) = intercalate " v " $ map show $ Set.toList ts
  show (TFieldUndefined) = "undefined"
  show (TFunc constraintsBefore paramTypes bodyType constraintsAfter) = "(" ++ showConstraints constraintsBefore ++ " " ++ intercalate " " (map show paramTypes) ++ " -> " ++ show bodyType ++ " " ++ showConstraints constraintsAfter ++ ")"



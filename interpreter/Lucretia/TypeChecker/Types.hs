{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}

module Lucretia.TypeChecker.Types where

import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Lucretia.TypeChecker.Definitions (Name, Param)

-- * Types (/Defition 2.1 (Types)/ in wp)

-- | @t_b@ /and/ @t@ in wp
data Type
  = TInt -- ^ @c@ in wp
  | TBool -- ^ @c@ in wp
  | TVar Name -- ^ @X@ in wp
  | TRec RecType -- ^ @t_r@ in wp
  | TOr (Set Type) -- ^ @t_b,1 v t_b,2@ in wp
  | TFieldUndefined -- ^ @_|_@ in wp
  | TFunc Constraints [Type] Type Constraints -- ^ @[t1, …, tn; Psi_1] => [tn+1; \Psi_2]@ in wp
  deriving (Eq, Ord)

-- * Record Type (@t_r = {l : t} | {}@ in wp)

-- | A mapping from Field Names to Types.
--
-- List of pairs @l : t@ in wp.
--
-- It is the same type as 'Env'
type RecType = Map Name Type

oneFieldTRec :: Name -> Type -> RecType
oneFieldTRec a t = Map.fromList [(a,t)]

emptyRecType :: RecType
emptyRecType = Map.empty


-- * Constraints (@Psi@ in wp)

-- | A mapping from Type Variable Names to Record Types.
--
-- List of pairs @X <# t_r@ in wp.
type Constraints = Map Name RecType

-- * Environment: Variable Names to Types (Sigma in wp)

-- | A mapping from Variable Names to Types.
--
-- It is the same type as 'Env'
type Env = Map Name Type

emptyEnv :: Env
emptyEnv = Map.empty

extendEnv :: Name -> Type -> Env -> Env
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

showRec :: RecType -> String 
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




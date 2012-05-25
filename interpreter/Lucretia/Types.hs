{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Lucretia.Types where

import Data.List(intercalate)
import Data.Map(Map)
import qualified Data.Map as Map

import Lucretia.TypeChecker.Definitions(Name, Param)

data Type
  = TInt
  | TBool
  | TVar Name
  | TRec RecType --TODO remove
  | TOr [Type] --TODO [Type] ~> Set Type
  | TFieldUndefined
  | TFunc Constraints [Type] Type Constraints
  deriving Eq
type RecType = Map Name Type
type Constraints = Map Name RecType

type Env = Map Name Type

data CheckState = CheckState { 
  cstCons :: Constraints,
  cstFresh :: [Int]
}
  deriving Eq

instance Show CheckState where
  show cst = showConstraints $ cstCons cst
  
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
  show (TOr ts) = intercalate " v " (map show ts)
  show (TFieldUndefined) = "undefined"
  show (TFunc constraintsBefore paramTypes bodyType constraintsAfter) = "(" ++ showConstraints constraintsBefore ++ " " ++ intercalate " " (map show paramTypes) ++ " -> " ++ show bodyType ++ " " ++ showConstraints constraintsAfter ++ ")"



oneFieldTRec :: Name -> Type -> RecType
oneFieldTRec a t = Map.fromList [(a,t)]


data TypeIso = TypeIso Type Constraints
data MapNameTypeIso = MapNameTypeIso Env Constraints
data NameIso = NameIso Name Constraints
data RecTypeIso = RecTypeIso RecType Constraints

instance Eq MapNameTypeIso where
  MapNameTypeIso e1 cs1 == MapNameTypeIso e2 cs2 = (toTypeIso e1 cs1) == (toTypeIso e2 cs2)
    where
    toTypeIso e cs = Map.map (\t -> TypeIso t cs) e

--Why *Iso types? To avoid rewriting the contents of "instance Eq (Map * *)".

--TODO introduce state: visited nodes, to avoid non-termination
--Problem with introducing state: how to have a state that is transferred between nodes compared in the code of "instance Eq (Map * *)" without rewriting this code.
  -- 1. solution (unclean): use global state / global variables
  -- 2. solution (not really answering above question): rewrite "instance Eq (Map * *)" using fold / State monad

instance Eq TypeIso where
  TypeIso (TVar n1) cs1 == TypeIso (TVar n2) cs2 = 
    NameIso n1 cs1 == NameIso n2 cs2
  TypeIso t1 _ == TypeIso t2 _ = 
    t1 == t2

instance Eq NameIso where
  NameIso n1 cs1 == NameIso n2 cs2 = RecTypeIso (cs1 `findOrFail` n1) cs1 == RecTypeIso (cs2 `findOrFail` n2) cs2
    where
    cs `findOrFail` n = case Map.lookup n cs of
			  Just result -> result
			  Nothing     -> error $ "Cannot find type variable named: " ++ n1 ++ " in constraints: " ++ showConstraints cs ++ "."

instance Eq RecTypeIso where
  RecTypeIso r1 cs1 == RecTypeIso r2 cs2 = MapNameTypeIso r1 cs1 == MapNameTypeIso r2 cs2


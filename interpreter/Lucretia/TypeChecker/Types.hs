{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Lucretia.TypeChecker.Types where

import Data.List(intercalate)
import Data.Map(Map)
import qualified Data.Map as Map

import Lucretia.TypeChecker.Syntax

data Type = TInt | TBool | TVar Name | TRec RecType | TOr Type Type | TFieldUndefined
  deriving Eq
type RecType = Map Name Type
type Constraints = Map Name Type
--TODO Type, OrList [Type]
--TODO adding constraint that exists results in error

type Env = Map Name Type

data CheckState = CheckState { 
  cstCons :: Constraints,
  cstFresh :: [Int]
}

instance Show CheckState where
  show cst = showConstraints $ cstCons cst
  
showConstraints cs = concat ["[",showFields fields,"]"] where
  fields = Map.toList cs
  showFields fields = intercalate ", " (map showField fields)
  showField (l,t) = concat [l," < ",show t]
  
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
  show (TOr t1 t2) = show t1 ++ " v " ++ show t2
  show (TFieldUndefined) = "undefined"


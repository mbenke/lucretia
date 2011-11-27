{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Lucretia.TypeChecker.Types where

import Data.Map(Map)
import qualified Data.Map as Map

import Lucretia.TypeChecker.Syntax

data Type = TInt | TVar Name | TRec RecType 
type RecType = (Map Name Type)
newtype Constraint = Constraint (Name, Type)
--TODO Type, OrList [Type]
--TODO adding constraint that exists results in error

type Env = Map Name Type

data CheckState = CheckState { 
  cstCons :: [Constraint],
  cstFresh :: [Int]
}

instance Show CheckState where
  show cst = show $ cstCons cst
  
instance Show Constraint where
  show (Constraint  (x,t)) = concat [x,"<",show t]
  
showRec :: RecType -> String 
showRec r = concat ["{",showFields fields,"}"] where
  fields = Map.toList r 
  showFields  = concatMap showField
  showField (l,t) = concat [l,":",show t]
  
instance Show Type where
  show TInt = "int"
  show (TRec r) = showRec r
  show (TVar v) = v


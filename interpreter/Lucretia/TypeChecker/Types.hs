{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Lucretia.TypeChecker.Types where

import Data.Map(Map)
import qualified Data.Map as Map

import Lucretia.TypeChecker.Syntax
import PrettyDecodablePrint
import Data.List(intercalate)

data Type = TInt | TVar Name | TRec RecType 
  deriving Show  
type RecType = (Map Name Type)
newtype Constraint = Constraint (Name, Type)
  deriving Show  
--TODO Type, OrList [Type]
--TODO adding constraint that exists results in error

type Env = Map Name Type

data CheckState = CheckState { 
  cstCons :: [Constraint],
  cstFresh :: [Int]
}

instance Show CheckState where
  show cst = show $ cstCons cst

instance ShowDecodable CheckState where
  pretty cst = pretty $ cstCons cst
  
instance ShowDecodable Constraint where
  pretty (Constraint (x,t)) = concat [x,"<", pretty t]
  
instance ShowDecodable Constraint => ShowDecodable [Constraint] where
  pretty xs = "[" ++ intercalate ", " (map pretty xs) ++ "]"

showRec :: RecType -> String 
showRec r = concat ["{",showFields fields,"}"] where
  fields = Map.toList r 
  showFields  = concatMap showField
  showField (l,t) = concat [l,":",show t]
  
instance ShowDecodable Type where
  pretty TInt = "int"
  pretty (TRec r) = showRec r
  pretty (TVar v) = v


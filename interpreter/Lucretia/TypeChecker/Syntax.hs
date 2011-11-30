module Lucretia.TypeChecker.Syntax where

type Name = String
type Program = Defs
type Defs = [Def]
type Def = (Name,Exp)
type Param = Name

-- TODO
-- full Exp tree
data Exp 
  = EInt Integer
  | EBoolTrue
  | EBoolFalse
  | EVar Name
  | ENew
  | ELet Name Exp Exp
  | ELets Defs Exp
  | EIf Exp Exp Exp
  | EGet Name Name
  | ESet Name Name Exp
  deriving Show  

programFromExp :: Exp -> Program
programFromExp e = [("_",e)]


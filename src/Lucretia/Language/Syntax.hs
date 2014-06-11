module Lucretia.Language.Syntax where

import Lucretia.Language.Types
import Lucretia.Language.Definitions (Var, Field, Label, Param)

type Program = Defs
type Defs = [Def]
type Def = (Var, Exp)

data Exp 
  = EInt Integer
  | EBool Bool
  | EStr String
  | ENone -- ^ () in wp
  | ENew

  | EAdd Exp Exp
  | EMul Exp Exp

  | EVar Var
  | ELet Var Exp Exp

  | EGet Var Field
  | ESet Var Field Exp

  | EIf Exp Exp Exp
  | EIfHasAttr Var Field Exp Exp

  | EFunDecl [Var] Exp
  | EFunCall Exp [Exp]
  
  deriving (Eq,Show)

programFromExp :: Exp -> Program
programFromExp e = [("_",e)]

instance Num Exp where
  fromInteger = EInt
  (+) = EAdd
  (*) = undefined
  (-) = undefined
  signum = undefined
  abs = undefined

